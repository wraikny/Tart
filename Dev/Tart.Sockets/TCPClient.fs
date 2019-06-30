namespace wraikny.Tart.Sockets.TCP

open System
open System.Linq
open System.Threading
open System.Net
open System.Net.Sockets
open System.Collections.Generic

open wraikny.Tart.Helper.Utils
open wraikny.Tart.Sockets


type Client<'SendMsg, 'RecvMsg> (encoder, decoder, socket, bufferSize) =
    let mutable socket : Socket = socket

    let bufferSize = bufferSize

    let encoder = encoder
    let decoder = decoder

    let sendQueue = new MsgQueue<'SendMsg>()
    let recvQueue = new MsgQueue<'RecvMsg>()

    let _lockObj = new Object()
    let mutable _isConnected = socket <> null

    let mutable cancel : CancellationTokenSource = null

    let mutable _debugDisplay = false

    new(encoder, decoder, bufferSize) =
        new Client<_, _>(encoder, decoder, null, bufferSize)


    abstract OnPopRecvMsg : 'RecvMsg -> unit
    default this.OnPopRecvMsg _ = ()

    member __.DebugDisplay
        with get() = _debugDisplay
        and  set(value) = _debugDisplay <- value

    member inline private this.DebugPrint(s) =
        if this.DebugDisplay then
            StaticLock.Printfn <|
                sprintf "[wraikny.Tart.Sockets.TCP.Client] %A" s


    member this.IsConnected
        with get() = lock _lockObj <| fun _ -> _isConnected
        and private set(value) = lock _lockObj <| fun _ -> _isConnected <- value


    interface IMsgQueue<'SendMsg> with
        member this.Enqueue(msg) =
            (sendQueue :> IMsgQueue<_>).Enqueue(msg)


    member internal this.Receive() =
        let rec loop xs =
            recvQueue.TryDequeue() |> function
            | Some(x) -> loop (x::xs)
            | None -> xs

        loop []


    member private this.DispatchSend() =
        if not this.IsConnected then
            raise <| InvalidOperationException()

        let rec loop() = async {
            match sendQueue.TryDequeue() with
            | Some msg ->
                this.DebugPrint(sprintf "AsyncSend: %A" msg)
                do! socket.AsyncSend(encoder msg) |> Async.Ignore
                return! loop()
            | None -> ()
        }

        loop()


    member private this.DispatchRecv() =
        if not this.IsConnected then
            raise <| InvalidOperationException()

        async {
            while socket.Poll(0, SelectMode.SelectRead) do
                let buffer = Array.zeroCreate<byte> bufferSize
                let! recvSize = socket.AsyncReceive(buffer)
                if recvSize = 0 then
                    this.Disconnect()
                elif recvSize > 0 then
                    decoder buffer
                    |> Option.iter(fun msg ->
                        this.DebugPrint(sprintf "AsyncReceive: %A" msg)
                        (recvQueue :> IMsgQueue<_>).Enqueue(msg)
                    )
        }


    member internal this.Dispatch() =
        if not this.IsConnected then
            raise <| InvalidOperationException()

        async {
            if socket.Poll(0, SelectMode.SelectWrite) then
                do! this.DispatchSend() |> Async.Ignore

            do! this.DispatchRecv()
        }

    
    member private this.Connect(ipEndpoint) =
        if this.IsConnected then
            raise <| InvalidOperationException()
    
        socket <- new Socket(AddressFamily.InterNetworkV6, SocketType.Stream, ProtocolType.Tcp)
        async {
            this.DebugPrint(sprintf "Connecting to %A" ipEndpoint)
            do! socket.AsyncConnect(ipEndpoint)
            this.DebugPrint(sprintf "Connected to server at %A" ipEndpoint)
            this.IsConnected <- true
        }


    member this.AsyncStart(ipEndpoint) =
        this.DebugPrint("Start")

        cancel <- new CancellationTokenSource()
        async {
            do! this.Connect(ipEndpoint)
            do! [|
                    async {
                        while true do
                            do! this.Dispatch()
                            Thread.Sleep(5)
                    }
                    async {
                        for msg in this.Receive() do
                            this.OnPopRecvMsg(msg)
                        Thread.Sleep(5)
                    }
                |] |> Async.Parallel |> Async.Ignore
        }
        |> fun a -> Async.Start(a, cancel.Token)

        this.DebugPrint("Started")


    member this.Disconnect() =
        this.DebugPrint("Disconnecting!")

        lock _lockObj <| fun _ ->
            if _isConnected then
                _isConnected <- false

                if cancel <> null then
                    cancel.Cancel()
                    cancel <- null

                socket.AsyncDisconnect(false)
                |> Async.RunSynchronously

                socket.Dispose()
                socket <- null

            else
                raise <| InvalidOperationException()

        this.DebugPrint("Disconnected!")


    interface IDisposable with
        member this.Dispose() =
            if this.IsConnected then
                this.Disconnect()