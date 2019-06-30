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


    new(encoder, decoder, bufferSize) =
        new Client<_, _>(encoder, decoder, null, bufferSize)


    member this.IsConnected
        with get() = lock _lockObj <| fun _ -> _isConnected
        and private set(value) = lock _lockObj <| fun _ -> _isConnected <- value


    member this.Send(bytes) =
        (sendQueue :> IMsgQueue<_>).Enqueue(bytes)


    member this.Receive() =
        let rec loop xs =
            recvQueue.TryPopMsg() |> function
            | Some(x) -> loop (x::xs)
            | None -> xs

        loop []


    member private this.DispatchSend() =
        if not this.IsConnected then
            raise <| InvalidOperationException()

        let rec loop() = async {
            match sendQueue.TryPopMsg() with
            | Some msg ->
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
                    |> Option.iter(fun msg -> (recvQueue :> IMsgQueue<_>).Enqueue(msg))
        }


    member this.Dispatch() =
        if not this.IsConnected then
            raise <| InvalidOperationException()

        async {
            if socket.Poll(0, SelectMode.SelectWrite) then
                do! this.DispatchSend() |> Async.Ignore

            do! this.DispatchRecv()
        }

    
    member this.Connect(ipEndpoint) =
        if this.IsConnected then
            raise <| InvalidOperationException()
    
        socket <- new Socket(AddressFamily.InterNetwork, SocketType.Stream, ProtocolType.Tcp)
        async {
            do! socket.AsyncConnect(ipEndpoint)
            this.IsConnected <- true
        }


    member this.AsyncStart(ipEndpoint) =
        cancel <- new CancellationTokenSource()
        async {
            do! this.Connect(ipEndpoint)
            while true do do! this.Dispatch()
        }
        |> fun a -> Async.Start(a, cancel.Token)


    member this.Disconnect() =
        lock _lockObj <| fun _ ->
            if _isConnected then
                _isConnected <- false

                if cancel <> null then
                    cancel.Cancel()
                    cancel <- null

                socket.Shutdown(SocketShutdown.Both)
                socket.Close()
                socket.Dispose()
                socket <- null

            else
                raise <| InvalidOperationException()


    interface IDisposable with
        member this.Dispose() =
            this.Disconnect()