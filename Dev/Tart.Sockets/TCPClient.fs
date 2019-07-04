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
    let aesBlockSize = 128 // 固定
    [<Literal>]
    let aesKeySize = 128 // 128/192/256bitから選択

    member private __.Encode(msg) =
        let bytes = encoder msg
        let encrypted = aes.CreateEncryptor().TransformFinalBlock(bytes, 0, bytes.Length)
        let size = encrypted |> Array.length |> uint16 |> BitConverter.GetBytes
        Array.append size encrypted


    member private __.Decode(bytes) =
        let decrypted = aes.CreateDecryptor().TransformFinalBlock(bytes, 0, bytes.Length)
        decoder decrypted


    member internal this.Socket
        with get() = socket
        and  set(value) = socket <- value

    member internal this.Cancel
        with get() = cancel
        and  set(value) = cancel <- value


    abstract OnPopRecvMsg : 'RecvMsg -> unit
    default __.OnPopRecvMsg _ = ()

    abstract OnDisconnected : unit -> unit
    default __.OnDisconnected() = ()

    abstract OnFailedToSend : 'SendMsg -> unit
    default __.OnFailedToSend _ = ()

    abstract OnFailedToReceive : unit -> unit
    default __.OnFailedToReceive() = ()


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
            | Some(x) -> loop (x::xs)
            | None -> xs

        loop []


    member inline private this.ReceiveOfSize(size) =
        async {
            let buffer = Array.zeroCreate<byte> (int size)
            let! _ = socket.AsyncReceive(buffer)
            return buffer
        }


    member internal this.InitCryptOfClient() =
        async {
            // RSA暗号を作成する
            let rsa = new RSACryptoServiceProvider()

            this.DebugPrint(sprintf "RSA:\n%s" <| rsa.ToXmlString(true))

            // RSAの公開鍵を送る
            let! _ =
                let pubKey = rsa.PublicKey 
                let length = pubKey.Length |> uint16 |> BitConverter.GetBytes
                this.Socket.AsyncSend(Array.append length pubKey)

            // RSAで暗号化された初期化ベクトルと共通鍵を受け取る
            let! encryptedLength = this.ReceiveOfSize(2)
            let! encrypted =
                this.ReceiveOfSize(BitConverter.ToUInt16(encryptedLength, 0))

            // 初期化ベクトルと共通鍵を復号化する
            let decrypted = rsa.Decrypt(encrypted, false)

            let ivLength = aesBlockSize / 8
            let keyLength = aesKeySize / 8
            let iv = decrypted.[0..ivLength-1]
            let key = decrypted.[ivLength..ivLength + keyLength - 1]
            // AESを作成する
            this.DebugPrint(sprintf "IV: %A" iv)
            this.DebugPrint(sprintf "Key: %A" key)
            try
            aes <-
                new AesCryptoServiceProvider(
                    BlockSize = aesBlockSize,
                    KeySize = aesKeySize,
                    Mode = CipherMode.CBC,
                    Padding = PaddingMode.PKCS7,
                    IV = iv,
                    Key = key
                )
            with e -> Console.WriteLine(e)
        }

    member internal this.InitCryptOfServer() =
        async {
            // 公開鍵を受信する
            let! pubKey = async {
                let! length = this.ReceiveOfSize(2)
                let! pubKey = this.ReceiveOfSize(BitConverter.ToInt16(length, 0))
                // return pubKey |> System.Convert.ToBase64String
                return pubKey |> Encoding.UTF8.GetString
            }

            // AESを作成する
            aes <-
                new AesCryptoServiceProvider(
                    BlockSize = aesBlockSize,
                    KeySize = aesKeySize,
                    Mode = CipherMode.CBC,
                    Padding = PaddingMode.PKCS7
                )
            aes.GenerateIV()
            aes.GenerateKey()
            
            // 初期化ベクトルと共通鍵を暗号化する
            use rsa = new RSACryptoServiceProvider()
            rsa.FromXmlString(pubKey)
            this.DebugPrint(sprintf "IV: %A" aes.IV)
            this.DebugPrint(sprintf "Key: %A" aes.Key)
            let encrypted = rsa.Encrypt(Array.append aes.IV aes.Key, false)
            let encryptedLength = encrypted.Length |> uint16 |> BitConverter.GetBytes

            // RSAで暗号化した初期化ベクトルと共通鍵を送信する
            do! socket.AsyncSend(Array.append encryptedLength encrypted) |> Async.Ignore
        }


    member private this.DispatchSend() =
        let rec loop() = async {
            match sendQueue.TryDequeue() with
            | Some msg ->
                this.DebugPrint(sprintf "Send: %A" msg)
                let! sendSize = socket.AsyncSend(this.Encode(msg))
                
                if sendSize = 0 then
                    this.OnFailedToSend(msg)
                return! loop()
            | None -> ()
        }

        loop()


    member private this.DispatchRecv() =
        let recv bufferSize f = async {
            let buffer = Array.zeroCreate<byte> bufferSize
            
            let! recvSize = socket.AsyncReceive(buffer)
            
            if recvSize > 0 then
                do! f(buffer)
            else
                this.OnFailedToReceive()
        }

        async {
            while socket.Poll(0, SelectMode.SelectRead) do
                do! recv sizeof<uint16> <| fun bytes -> async {
                    let size = BitConverter.ToInt16(bytes, 0)
                    do! recv (int size) <| fun bytes -> async {
                        this.Decode(bytes)
                        |> Option.iter(fun msg ->
                            this.DebugPrint(sprintf "Receive: %A" msg)
                            (recvQueue :> IMsgQueue<_>).Enqueue(msg)
                        )
                    }
                }
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
        lock _lockObj <| fun _ ->
            if _isConnected then
                this.DebugPrint("Disconnecting")
                _isConnected <- false

                if cancel <> null then
                    cancel.Cancel()
                    cancel <- null

                socket.Dispose()
                socket <- null

                aes <- null

                this.OnDisconnected()
                this.DebugPrint("Disconnected")
                true
            else
                this.DebugPrint("Already disconnected")
                false


    interface IClientHandler<'SendMsg> with
        member this.IsConnected with get() = this.IsConnected

        // member this.SendSync(msg) = this.Send(msg)

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


    abstract OnConnecting : unit -> unit
    default __.OnConnecting() = ()

    abstract OnConnected : unit -> unit
    default __.OnConnected() = ()


    member private this.Connect(ipEndpoint) =
        if this.IsConnected then
            raise <| InvalidOperationException()
    
        this.Socket <- new Socket(AddressFamily.InterNetworkV6, SocketType.Stream, ProtocolType.Tcp)
        async {
            this.DebugPrint(sprintf "Connecting to %A" ipEndpoint)

            do! this.Socket.AsyncConnect(ipEndpoint)

            do! this.InitCryptOfClient()

            this.IsConnected <- true

            this.DebugPrint(sprintf "Connected to server at %A" ipEndpoint)
        }
    
    interface IClient<'SendMsg> with
        member this.IsConnected with get() = this.IsConnected

        member this.StartAsync(ipEndpoint) =
            this.DebugPrint("Start")

            this.Cancel <- new CancellationTokenSource()
            async {
                this.OnConnecting()
                do! this.Connect(ipEndpoint)
                this.OnConnected()

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
                                    this.OnPopRecvMsg(msg)
                                Thread.Sleep(5)
                        }
                    |] |> Async.Parallel |> Async.Ignore
            }
            |> fun a -> Async.Start(a, this.Cancel.Token)

            this.DebugPrint("Started")

        // member this.Disconnect() = this.Disconnect()

        // member this.Send(msg) = this.Send(msg)