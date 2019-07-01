namespace wraikny.Tart.Sockets.TCP

open System
open System.Linq
open System.Threading
open System.Net
open System.Net.Sockets
open System.Collections.Generic

open wraikny.Tart.Helper.Utils
open wraikny.Tart.Sockets


type ClientBase<'SendMsg, 'RecvMsg> internal (encoder, decoder, socket) =
    let mutable socket : Socket = socket

    let encoder msg =
        let bytes = encoder msg
        let size = bytes |> Array.length |> BitConverter.GetBytes
        Array.append size bytes

    let decoder msg =
        decoder msg

    let sendQueue = new MsgQueue<'SendMsg>()
    let recvQueue = new MsgQueue<'RecvMsg>()

    let _lockObj = new Object()

    let mutable _isConnected = socket <> null

    let mutable cancel : CancellationTokenSource = null

    let mutable _debugDisplay = false

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

    member private this.DispatchSend() =
        let rec loop() = async {
            match sendQueue.TryDequeue() with
            | Some msg ->
                this.DebugPrint(sprintf "Send: %A" msg)
                let! sendSize = this.Socket.AsyncSend(encoder msg)
                
                if sendSize = 0 then
                    this.Disconnect() |> ignore
                return! loop()
            | None -> ()
        }

        loop()


    member private this.DispatchRecv() =
        let recv bufferSize f = async {
            let buffer = Array.zeroCreate<byte> bufferSize
            
            let! recvSize = socket.AsyncReceive(buffer)
            
            if recvSize > 0 then do! f(buffer)
        }

        async {
            while socket.Poll(0, SelectMode.SelectRead) do
                do! recv 4 <| fun bytes -> async {
                    let size = BitConverter.ToInt32(bytes, 0)
                    do! recv size <| fun bytes -> async {
                        decoder bytes
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
                do! this.DispatchSend() |> Async.Ignore

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