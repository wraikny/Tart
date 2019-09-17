namespace wraikny.Tart.Socket.TCP

open System
open System.Linq
open System.Threading
open System.Net
open System.Net.Sockets
open System.Collections.Generic

open wraikny.Tart.Helper.Collections
open wraikny.Tart.Socket
open wraikny.Tart.Socket.Crypt


open System.Security.Cryptography
open System.Text

open FSharpPlus


type ClientBase<'SendMsg, 'RecvMsg> internal (encoder, decoder, socket) =
    let mutable socket : Socket = socket
    let mutable clientId : ClientID option = None

    let encoder = encoder
    let decoder = decoder

    let sendQueue = new MsgQueue<'SendMsg>() :> IQueue<_>
    let recvQueue = new MsgQueue<'RecvMsg>() :> IQueue<_>

    let _lockObj = Object()

    let mutable _isConnected = not <| isNull socket

    let mutable cancel : CancellationTokenSource = null

    let mutable aes : AesCryptoServiceProvider = null

    let mutable _debugDisplay = false

    let onReceiveMsg = new Event<'RecvMsg>()
    let onDisconnected = new Event<unit>()
    let onError = new Event<exn>()

    [<Literal>]
    let RsaKeySize = 1024
    [<Literal>]
    let AesBlockSize = 128 // fixed
    [<Literal>]
    let AesKeySize = 128 // select from { 128bit, 192bit, 256bit }

    member __.OnReceiveMsg with get() = onReceiveMsg.Publish
    member __.OnDisconnected with get() = onDisconnected.Publish
    member __.OnError with get() = onError.Publish

    member __.ClientId
        with get() = clientId
        and  internal set(value) = clientId <- value

    member private __.Encode(msg : 'SendMsg SocketMsg) =
        let bytes = msg.Encode(encoder)
        aes.Encrypt(bytes)


    member private __.Decode(bytes) : 'RecvMsg SocketMsg option =
        let decrypted = aes.Decrypt(bytes)
        SocketMsg<'RecvMsg>.Decode(decoder, decrypted)


    member internal this.Socket
        with get() = socket
        and  set(value) = socket <- value

    member internal this.Cancel
        with get() = cancel
        and  set(value) = cancel <- value

    member internal __.CallOnErrorEvent(e) = onError.Trigger(e)


    member __.DebugDisplay
        with get() = _debugDisplay
        and  set(value) = _debugDisplay <- value

    member private this.DebugPrint(s) =
        if this.DebugDisplay then
            Console.WriteLine(sprintf "[wraikny.Tart.Sockets.TCP.ClientBase] %A" s)


    member this.IsConnected
        with get() = lock _lockObj <| fun _ -> _isConnected
        and internal set(value) = lock _lockObj <| fun _ -> _isConnected <- value


    member internal this.Receive() =
        let rec loop() = async {
            try
                match recvQueue.TryDequeue() with
                | Some(x) ->
                    onReceiveMsg.Trigger(x)
                    return! (loop ())
                | None -> return ()
            with e ->
                this.CallOnErrorEvent(e)
                (this :> IDisposable).Dispose()
        }

        loop()


    member inline internal this.ReceiveOfSize(size) =
        async {
            let buffer = Array.zeroCreate<byte> (int size)
            let! recvSize = socket.AsyncReceive(buffer)
            return buffer, recvSize
        }

    member internal this.ReceiveWithHead() =
        async {
            let! length, recvSize = this.ReceiveOfSize(2)
            if recvSize = 0 then
                return (empty, 0)
            else
                let length = BitConverter.ToUInt16(length, 0)
                return! this.ReceiveOfSize(length)
        }

    member internal this.DecryptedReceiveWithHead() =
        async {
            let! bytes, recvSize = this.ReceiveWithHead()
            return (aes.Decrypt(bytes), recvSize)
        }

    member internal this.SendWithHead(bytes) =
        async {
            let length = bytes |> length |> uint16 |> BitConverter.GetBytes
            return! socket.AsyncSend(length <|> bytes)
        }

    member internal this.EncryptedSendWithHead(bytes) =
        async {
            return! this.SendWithHead(aes.Encrypt(bytes))
        }

    // --------------------------------------------------
    // Begin: Diffie-Hellman key exchang
    // --------------------------------------------------
    member internal this.InitCryptOfClient() =
        this.DebugPrint("Start Crypt")
        async {
            try
                // 1. Create RSA
                use rsa = new RSACryptoServiceProvider(RsaKeySize)

                this.DebugPrint(sprintf "RSA:\n%s" <| rsa.ToXmlStringForDotNetCore(true))

                // 2a. Send Public Key of RSA
                let! sendSize = this.SendWithHead(rsa.PublicKey)

                if sendSize <= 0 then
                    failwithf "Failed to Send RSA public Key"

                let! iv, key = async {
                    // 5b. Receive AES's IV and Key which are encrypted by RSA public key
                    let! encrypted, recvSize = this.ReceiveWithHead()

                    if recvSize <= 0 then
                        failwith "Failed to received AES Data"

                    this.DebugPrint(sprintf "Encrypted Key: %A", encrypted)

                    // 6. Decrypted IV and Key
                    let decrypted = rsa.Decrypt(encrypted, false)

                    this.DebugPrint(sprintf "Decrypted Key: %A", decrypted)

                    let ivLength = AesBlockSize / 8

                    return decrypted |> Array.splitAt ivLength
                }

                this.DebugPrint(sprintf "IV: %A" iv)
                this.DebugPrint(sprintf "Key: %A" key)
            
                // 7. Create AES
                aes <-
                    new AesCryptoServiceProvider(
                        BlockSize = AesBlockSize,
                        KeySize = key.Length * 8,
                        Mode = CipherMode.CBC,
                        Padding = PaddingMode.PKCS7,
                        IV = iv,
                        Key = key
                    )
                this.DebugPrint(sprintf "Created AES")
            with e ->
                this.CallOnErrorEvent(e)
                (this :> IDisposable).Dispose()
        }

    member internal this.InitCryptOfServer() =
        this.DebugPrint("Start Crypt")
        async {
            try
                // 2b. Receive Public Key of RSA
                let! pubKey = async {
                    let! pubKey, recvSize = this.ReceiveWithHead()
                    if recvSize <= 0 then
                        failwithf "Failed to receive RSA Public Key of %A" clientId
                    return pubKey |> Encoding.UTF8.GetString
                }

                this.DebugPrint(sprintf "Received Public Key")

                // 3. Creat AES
                aes <-
                    new AesCryptoServiceProvider(
                        BlockSize = AesBlockSize,
                        KeySize = AesKeySize,
                        Mode = CipherMode.CBC,
                        Padding = PaddingMode.PKCS7
                    )

                this.DebugPrint(sprintf "Created AES")

                aes.GenerateIV()
                aes.GenerateKey()

                this.DebugPrint(sprintf "IV: %A" aes.IV)
                this.DebugPrint(sprintf "Key: %A" aes.Key)
            
                // 4. Encrypt IV and Key of AES
                use rsa = new RSACryptoServiceProvider()
                rsa.FromXmlStringForDotNetCore(pubKey)

                this.DebugPrint(sprintf "Encrypted with RSA")

                let encrypted = rsa.Encrypt(aes.IV <|> aes.Key, false)

                // 5a. Send encrypted iv and key
                let! sendSize = this.SendWithHead(encrypted)

                if sendSize <= 0 then
                    failwithf "Failed to send AES Data of Client %A" clientId

                this.DebugPrint(sprintf "Send AES Data")

            with e ->
                this.CallOnErrorEvent(e)
                (this :> IDisposable).Dispose()
        }
    // --------------------------------------------------
    // End: Diffie-Hellman key exchang
    // --------------------------------------------------



    member this.SendMsgAsync(msg) =
        async {
            this.DebugPrint(sprintf "Send: %A" msg)
            try
                let! sendSize = this.SendWithHead(this.Encode(UserMsg msg))
                ()
                if sendSize <= 0 then
                    failwith "SendSize <= 0"
            with e ->
                this.CallOnErrorEvent(e)
                (this :> IDisposable).Dispose()
        }

    member private this.DispatchSend() =
        let rec loop() = async {
            match sendQueue.TryDequeue() with
            | Some msg ->
                do! this.SendMsgAsync(msg)

                return! loop()
            | None -> ()
        }

        loop()


    member private this.DispatchRecv() = async {
        try
        while socket.Poll(0, SelectMode.SelectRead) do
            let! bytes, recvSize = this.ReceiveWithHead()
            if recvSize > 1 then
                match this.Decode(bytes) with
                | Some(socketMsg) ->
                    match socketMsg with
                    | UserMsg recvMsg ->
                        this.DebugPrint(sprintf "Receive: %A" recvMsg)
                        (recvQueue :> IQueue<_>).Enqueue(recvMsg)

                | None -> ()
            else
                failwith "ReceiveSize <= 1"
        with e ->
            this.CallOnErrorEvent(e)
            (this :> IDisposable).Dispose()
    }


    member internal this.Dispatch() =
        if not this.IsConnected then
            raise <| InvalidOperationException()

        async {
            try
                if socket.Poll(0, SelectMode.SelectWrite) then
                    do! this.DispatchSend()

                do! this.DispatchRecv()
            with e ->
                this.CallOnErrorEvent(e)
                (this :> IDisposable).Dispose()
        }

    member internal this.Disconnect() : bool =
        let disconnectable =
            lock _lockObj <| fun _ ->
                if _isConnected then
                    _isConnected <- false
                    true
                else
                    false

        if disconnectable then
            try
                this.DebugPrint("Disconnecting")

                if not <| isNull cancel then
                    cancel.Cancel()
                cancel <- null

                if not <| isNull socket then
                    socket.Dispose()
                socket <- null

                this.ClientId <- None

                aes.Dispose()
                aes <- null

                onDisconnected.Trigger()

                this.DebugPrint("Disconnected")
            with e ->
                this.CallOnErrorEvent(e)
        else
            this.DebugPrint("Already disconnected")

        disconnectable

    member __.Enqueue(msg) =
        sendQueue.Enqueue(msg)

    //member __.TryDequeue() =
    //    recvQueue.TryDequeue()

    interface IClientHandler<'SendMsg> with
        member this.IsConnected with get() = this.IsConnected

        // member this.SendSync(msg) = this.Send(msg)
        member this.SendMsgAsync(msg) = this.SendMsgAsync(msg)

        member this.Enqueue(msg) =
            this.Enqueue(msg)

        //member this.TryDequeue() =
        //    this.TryDequeue()
            

    interface IDisposable with
        member this.Dispose() =
            this.DebugPrint("Dispose")
            this.Disconnect() |> ignore


type Client<'SendMsg, 'RecvMsg>(encoder, decoder) =
    inherit ClientBase<'SendMsg, 'RecvMsg>(encoder, decoder, null)

    let onConnected = new Event<unit>()

    let mutable isHandlingRecvMsgs = true
    let _isHandlingRecvMsgsLockObj = new System.Object()

    member __.OnConnected with get() = onConnected.Publish

    member private this.DebugPrint(s) =
        if this.DebugDisplay then
            Console.WriteLine(sprintf "[wraikny.Tart.Sockets.TCP.Client] %A" s)

    member private this.Connect(ipEndpoint) =
        if this.IsConnected then
            raise <| InvalidOperationException()
    
        this.Socket <- new Socket(AddressFamily.InterNetworkV6, SocketType.Stream, ProtocolType.Tcp)
        async {
            this.DebugPrint(sprintf "Connecting to %A" ipEndpoint)
            try
                do! this.Socket.AsyncConnect(ipEndpoint)

                this.DebugPrint("Connection Accepted")

                do! this.InitCryptOfClient()

                this.DebugPrint("Exchanged Crypt Key")

                let! clientId, _ = this.DecryptedReceiveWithHead()
                this.ClientId <- Some <| BitConverter.ToUInt32(clientId, 0)
                this.DebugPrint(sprintf "ClientId: %A" this.ClientId)

                this.IsConnected <- true

                this.DebugPrint(sprintf "Connected to server at %A" ipEndpoint)
            with e ->
                this.CallOnErrorEvent(e)
                (this :> IDisposable).Dispose()
        }


    
    interface IClient<'SendMsg, 'RecvMsg> with
        member this.ClientId with get() = this.ClientId.Value

        member this.IsConnected with get() = this.IsConnected

        member __.IsHandlingRecvMsgs
            with get() = lock _isHandlingRecvMsgsLockObj <| fun() -> isHandlingRecvMsgs
            and set(x) = lock _isHandlingRecvMsgsLockObj <| fun() -> isHandlingRecvMsgs <- x

        member this.StartAsync(ipEndpoint) =
            this.DebugPrint("Start")

            this.Cancel <- new CancellationTokenSource()
            async {
                try
                    do! this.Connect(ipEndpoint)
                    onConnected.Trigger()

                    return! [|
                        async {
                            while true do
                                do! this.Dispatch()
                                Thread.Sleep(5)
                        }
                        async {
                            while true do
                                if (this :> IClient<_, _>).IsHandlingRecvMsgs then
                                    do! this.Receive()
                                Thread.Sleep(5)
                        }
                    |] |> Async.Parallel |> Async.Ignore
                with e ->
                    this.CallOnErrorEvent(e)
                    (this :> IDisposable).Dispose()
            }
            |> fun a -> Async.Start(a, this.Cancel.Token)

            this.DebugPrint("Started")

        member __.OnReceiveMsg with get() = base.OnReceiveMsg
        member __.OnDisconnected with get() = base.OnDisconnected
        member __.OnError with get() = base.OnError

        // member this.Disconnect() = this.Disconnect()

        // member this.Send(msg) = this.Send(msg)

        member this.Enqueue(msg) = this.Enqueue(msg)