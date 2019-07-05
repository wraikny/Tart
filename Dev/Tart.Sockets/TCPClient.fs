namespace wraikny.Tart.Sockets.TCP

open System
open System.Linq
open System.Threading
open System.Net
open System.Net.Sockets
open System.Collections.Generic

open wraikny.Tart.Helper.Utils
open wraikny.Tart.Sockets
open wraikny.Tart.Sockets.Crypt


open System.Security.Cryptography
open System.Text


type ClientBase<'SendMsg, 'RecvMsg> internal (encoder, decoder, socket) =
    let mutable socket : Socket = socket
    let mutable clientId : ClientID option = None

    let encoder = encoder
    let decoder = decoder

    let sendQueue = new MsgQueue<'SendMsg>()
    let recvQueue = new MsgQueue<'RecvMsg>()

    let _lockObj = new Object()

    let mutable _isConnected = socket <> null

    let mutable cancel : CancellationTokenSource = null

    let mutable aes : AesCryptoServiceProvider = null

    let mutable _debugDisplay = false

    [<Literal>]
    let rsaKeySize = 1024
    [<Literal>]
    let aesBlockSize = 128 // fixed
    [<Literal>]
    let aesKeySize = 128 // select from { 128bit, 192bit, 256bit }

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


    abstract OnPopRecvMsgAsync : 'RecvMsg -> Async<unit>
    default __.OnPopRecvMsgAsync _ = async { () }

    abstract OnDisconnected : unit -> unit
    default __.OnDisconnected() = ()

    abstract OnFailedToSend : 'SendMsg -> Async<unit>
    default __.OnFailedToSend _ = async { () }

    abstract OnFailedToReceive : unit -> Async<unit>
    default __.OnFailedToReceive() = async { () }


    member __.DebugDisplay
        with get() = _debugDisplay
        and  set(value) = _debugDisplay <- value

    member inline private this.DebugPrint(s) =
        if this.DebugDisplay then
            Console.WriteLine(sprintf "[wraikny.Tart.Sockets.TCP.ClientBase] %A" s)


    member this.IsConnected
        with get() = lock _lockObj <| fun _ -> _isConnected
        and internal set(value) = lock _lockObj <| fun _ -> _isConnected <- value


    member internal this.Receive() =
        let rec loop xs =
            recvQueue.TryDequeue() |> function
            | Some(x) ->
                loop (x::xs)
            | None -> xs

        loop []


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
                return (Array.empty, 0)
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
            let length = bytes |> Array.length |> uint16 |> BitConverter.GetBytes
            return! socket.AsyncSend(Array.append length bytes)
        }

    member internal this.EncryptedSendWithHead(bytes) =
        async {
            return! this.SendWithHead(aes.Encrypt(bytes))
        }

    // --------------------------------------------------
    // Begin: Diffie-Hellman key exchang
    // --------------------------------------------------
    member internal this.InitCryptOfClient() =
        async {
            // 1. Create RSA
            use rsa = new RSACryptoServiceProvider(rsaKeySize)

            this.DebugPrint(sprintf "RSA:\n%s" <| rsa.ToXmlString(true))

            // 2a. Send Public Key of RSA
            let! _ = this.SendWithHead(rsa.PublicKey)

            let! iv, key = async {
                // 5b. Receive AES's IV and Key which are encrypted by RSA public key
                let! encrypted, _ = this.ReceiveWithHead()

                // 6. Decrypted IV and Key
                let decrypted = rsa.Decrypt(encrypted, false)

                let ivLength = aesBlockSize / 8

                return decrypted |> Array.splitAt ivLength
            }

            this.DebugPrint(sprintf "IV: %A" iv)
            this.DebugPrint(sprintf "Key: %A" key)
            
            // 7. Create AES
            aes <-
                new AesCryptoServiceProvider(
                    BlockSize = aesBlockSize,
                    KeySize = key.Length * 8,
                    Mode = CipherMode.CBC,
                    Padding = PaddingMode.PKCS7,
                    IV = iv,
                    Key = key
                )
        }

    member internal this.InitCryptOfServer() =
        async {
            // 2b. Receive Public Key of RSA
            let! pubKey = async {
                let! pubKey, _ = this.ReceiveWithHead()
                return pubKey |> Encoding.UTF8.GetString
            }

            // 3. Creat AES
            aes <-
                new AesCryptoServiceProvider(
                    BlockSize = aesBlockSize,
                    KeySize = aesKeySize,
                    Mode = CipherMode.CBC,
                    Padding = PaddingMode.PKCS7
                )
            aes.GenerateIV()
            aes.GenerateKey()
            
            // 4. Encrypt IV and Key of AES
            use rsa = new RSACryptoServiceProvider()
            rsa.FromXmlString(pubKey)
            this.DebugPrint(sprintf "IV: %A" aes.IV)
            this.DebugPrint(sprintf "Key: %A" aes.Key)

            let encrypted = rsa.Encrypt(Array.append aes.IV aes.Key, false)

            // 5a. Send encrypted iv and key
            do! this.SendWithHead(encrypted) |> Async.Ignore
        }
    // --------------------------------------------------
    // End: Diffie-Hellman key exchang
    // --------------------------------------------------



    member this.SendMsgAsync(msg) =
        async {
            this.DebugPrint(sprintf "Send: %A" msg)
            let! sendSize = this.SendWithHead(this.Encode(UserMsg msg))
            
            if sendSize <= 0 then
                this.DebugPrint("Send Size <= 0")
                do! this.OnFailedToSend(msg)
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
        while socket.Poll(0, SelectMode.SelectRead) do
            let! bytes, recvSize = this.ReceiveWithHead()
            if recvSize <= 0 then
                this.DebugPrint("Receive Size <= 0")
                do! this.OnFailedToReceive()
            elif not (bytes.Length > 1) then
                this.DebugPrint("Receive Bytes Length <= 1")
                do! this.OnFailedToReceive()
            else
                match this.Decode(bytes) with
                | Some(socketMsg) ->
                    match socketMsg with
                    | UserMsg recvMsg ->
                        this.DebugPrint(sprintf "Receive: %A" recvMsg)
                        (recvQueue :> IMsgQueue<_>).Enqueue(recvMsg)

                | None -> ()
    }


    member internal this.Dispatch() =
        if not this.IsConnected then
            raise <| InvalidOperationException()

        async {
            if socket.Poll(0, SelectMode.SelectWrite) then
                do! this.DispatchSend()

            do! this.DispatchRecv()
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
            this.DebugPrint("Disconnecting")

            if cancel <> null then
                cancel.Cancel()
            cancel <- null

            if socket <> null then
                socket.Dispose()
            socket <- null

            this.ClientId <- None

            aes.Dispose()
            aes <- null

            this.OnDisconnected()

            this.DebugPrint("Disconnected")
        else
            this.DebugPrint("Already disconnected")

        disconnectable


    interface IClientHandler<'SendMsg> with
        member this.IsConnected with get() = this.IsConnected

        // member this.SendSync(msg) = this.Send(msg)
        member this.SendMsgAsync(msg) = this.SendMsgAsync(msg)

    interface IMsgQueue<'SendMsg> with
        member this.Enqueue(msg) =
            (sendQueue :> IMsgQueue<_>).Enqueue(msg)


    interface IDisposable with
        member this.Dispose() =
            this.DebugPrint("Dispose")
            this.Disconnect() |> ignore


[<AbstractClass>]
type Client<'SendMsg, 'RecvMsg>(encoder, decoder) =
    inherit ClientBase<'SendMsg, 'RecvMsg>(encoder, decoder, null)

    member inline private this.DebugPrint(s) =
        if this.DebugDisplay then
            Console.WriteLine(sprintf "[wraikny.Tart.Sockets.TCP.Client] %A" s)


    abstract OnConnecting : unit -> Async<unit>
    default __.OnConnecting() = async { () }

    abstract OnConnected : unit -> Async<unit>
    default __.OnConnected() = async { () }


    member private this.Connect(ipEndpoint) =
        if this.IsConnected then
            raise <| InvalidOperationException()
    
        this.Socket <- new Socket(AddressFamily.InterNetworkV6, SocketType.Stream, ProtocolType.Tcp)
        async {
            this.DebugPrint(sprintf "Connecting to %A" ipEndpoint)

            do! this.Socket.AsyncConnect(ipEndpoint)
            do! this.InitCryptOfClient()

            let! clientId, _ = this.DecryptedReceiveWithHead()
            this.ClientId <- Some <| BitConverter.ToUInt32(clientId, 0)
            this.DebugPrint(sprintf "ClientId: %A" this.ClientId)

            this.IsConnected <- true

            this.DebugPrint(sprintf "Connected to server at %A" ipEndpoint)
        }
    
    interface IClient<'SendMsg> with
        member this.IsConnected with get() = this.IsConnected

        member this.StartAsync(ipEndpoint) =
            this.DebugPrint("Start")

            this.Cancel <- new CancellationTokenSource()
            async {
                do! this.OnConnecting()
                do! this.Connect(ipEndpoint)
                do! this.OnConnected()

                return! [|
                        async {
                            let dispatch = this.Dispatch()
                            while true do
                                do! dispatch
                                Thread.Sleep(5)
                        }
                        async {
                            while true do
                                for msg in this.Receive() do
                                    do! this.OnPopRecvMsgAsync(msg)
                                Thread.Sleep(5)
                        }
                    |] |> Async.Parallel |> Async.Ignore
            }
            |> fun a -> Async.Start(a, this.Cancel.Token)

            this.DebugPrint("Started")

        // member this.Disconnect() = this.Disconnect()

        // member this.Send(msg) = this.Send(msg)