    namespace wraikny.Tart.Sockets.TCP

    open System
    open System.Linq
    open System.Threading
    open System.Net
    open System.Net.Sockets
    open System.Collections.Generic

    open wraikny.Tart.Helper.Utils
    open wraikny.Tart.Sockets


    type ClientHandler(socket, bufferSize) =
        let socket : Socket = socket

        let bufferSize = bufferSize

        let sendQueue = new MsgQueue<byte []>()
        let recvQueue = new MsgQueue<byte []>()

        let mutable _isConnected = true
        member this.IsConnected
            with get() = _isConnected
            and private set(value) = _isConnected <- value

        member this.Send(bytes) =
            (sendQueue :> IMsgQueue<_>).Enqueue(bytes)


        member this.Receive() =
            let rec loop xs =
                recvQueue.TryPopMsg() |> function
                | Some(x) -> loop (x::xs)
                | None -> xs
            loop []


        member private this.DispatchSend() =
            let rec loop() = async {
                match sendQueue.TryPopMsg() with
                | Some bytes ->
                    do! socket.AsyncSend(bytes) |> Async.Ignore
                    return! loop()
                | None -> ()
            }
            loop()


        member private this.DispatchRecv() =
            async {
                while socket.Poll(0, SelectMode.SelectRead) do
                    let buffer = [|for _ in 1..bufferSize -> 0uy|]
                    let! recvSize = socket.AsyncReceive(buffer)
                    if recvSize = 0 then
                        this.Disconnect()
                    elif recvSize > 0 then
                        (recvQueue :> IMsgQueue<_>).Enqueue(buffer)
            }

        member this.Dispatch() =
            async {
                if socket.Poll(0, SelectMode.SelectWrite) then
                    do! this.DispatchSend() |> Async.Ignore

                do! this.DispatchRecv()
            }

        member this.Disconnect() =
            this.IsConnected <- false
            socket.Shutdown(SocketShutdown.Both)
            socket.Close()


        interface IDisposable with
            member this.Dispose() =
                if this.IsConnected then
                    this.Disconnect()



    [<AbstractClass>]
    type ServerBase<'SendMsg, 'RecvMsg>(decoder, bufferSize, endpoint) =
        let mutable nextClientID : ClientID = LanguagePrimitives.GenericZero

        let _lockObj = new Object()
        let clients = new Dictionary<ClientID, ClientHandler>()
        let endpoint = endpoint
        let mutable listener : Socket = null

        let decoder : byte[] -> 'RecvMsg option = decoder
        
        let bufferSize : int = bufferSize

        let receiveQueue = new MsgQueue<ClientID * 'RecvMsg>()
        let sendQueue = new MsgQueue<'SendMsg>()


        let mutable cancelAccepting = null
        let mutable cancelMessaging = null

        new (decoder, bufferSize, port, ?ipAddress) =
            let ipAddress = defaultArg ipAddress IPAddress.Any
            new ServerBase<_, _>(decoder, bufferSize, IPEndPoint(ipAddress, port))


        member internal this.Clients with get() = clients

        abstract OnConnectedClientAsync : ClientID -> Async<unit>
        abstract OnPopReceiveMsgAsync : ClientID * 'RecvMsg -> Async<unit>
        abstract OnPopSendMsgAsync : 'SendMsg -> Async<unit>

        member private this.AsyncClientsDispatch() =
            let removeIds = new List<ClientID>()

            let rec loop() = async {
                let clientsList = lock _lockObj <| fun _ -> clients.ToList()
                for i in clientsList do
                    let client = i.Value
                    do! client.Dispatch()
                    if not client.IsConnected then
                        removeIds.Add(i.Key)

                lock _lockObj <| fun _ ->
                    for id in removeIds do
                        clients.Remove(id) |> ignore

                removeIds.Clear()

                Thread.Sleep(5)
                return! loop()
            }

            loop()


        member private this.AsyncReceive() =
            let rec loop() = async {
                let clientsList = lock _lockObj <| fun _ -> clients.ToList()

                for i in clientsList do
                    let client = i.Value
                    for r in client.Receive() do
                        decoder r |> function
                        | Some(msg) ->
                            (receiveQueue :> IMsgQueue<_>).Enqueue(i.Key,  msg)
                        | None -> ()
                
                return! loop()
            }
        
            loop()


        member private this.AsyncPopReceiveMsg() =
            let rec loop() = async {
                match receiveQueue.TryPopMsg() with
                | Some(msg) ->
                    do! this.OnPopReceiveMsgAsync(msg)
                | None -> ()
                return! loop()
            }
            loop()

        member private this.AsyncPopSendMsg() =
            let rec loop() = async {
                match sendQueue.TryPopMsg() with
                | Some(msg) ->
                    do! this.OnPopSendMsgAsync(msg)
                | None -> ()
                return! loop()
            }

            loop()


        member private this.IServer with get() = this :> IServer<_>


        interface IServer<'SendMsg> with
            member this.IsAccepting with get() = cancelAccepting <> null

            member this.IsMessaging with get() = cancelMessaging <> null
            
            member this.StartAccepting() =
                if this.IServer.IsAccepting then
                    raise <| InvalidOperationException()

                cancelAccepting <- new CancellationTokenSource()

                listener <- new Socket(AddressFamily.InterNetwork, SocketType.Stream, ProtocolType.Tcp)
                
                listener.Bind(endpoint)
                listener.Listen(int SocketOptionName.MaxConnections)
                
                let rec loop() = async {
                    let! client = listener.AsyncAccept()
                
                    do! this.OnConnectedClientAsync(nextClientID)
                
                    lock _lockObj <| fun _ ->
                        clients.Add(nextClientID, new ClientHandler(client, bufferSize) )
                        nextClientID <- nextClientID + LanguagePrimitives.GenericOne
                
                    return! loop()
                }
                
                loop()
                |> fun a -> Async.Start(a, cancelAccepting.Token)


            member this.StopAccepting() =
                if not this.IServer.IsAccepting then
                    raise <| InvalidOperationException()

                cancelAccepting.Cancel()

                listener.Close()
                listener <- null
                
                cancelAccepting <- null


            member this.StartMessaging() =
                if this.IServer.IsMessaging then
                    raise <| InvalidOperationException()

                cancelMessaging <- new CancellationTokenSource()

                [|
                    this.AsyncClientsDispatch()
                    this.AsyncReceive()
                    this.AsyncPopReceiveMsg()
                    this.AsyncPopSendMsg()
                |]
                |> Async.Parallel
                |> Async.Ignore
                |> fun a -> Async.Start(a, cancelMessaging.Token)


            member this.StopMessaging() =
                if not this.IServer.IsMessaging then
                    raise <| InvalidOperationException()

                cancelMessaging.Cancel()

                cancelMessaging <- null


        interface IMsgQueue<'SendMsg> with
            member this.Enqueue(msg) =
                (sendQueue :> IMsgQueue<_>).Enqueue(msg)
    

        interface IDisposable with
            member this.Dispose() =
                for c in clients do
                    (c.Value :> IDisposable).Dispose()

                if this.IServer.IsAccepting then
                    this.IServer.StopAccepting()

                if this.IServer.IsMessaging then
                    this.IServer.StopMessaging()

