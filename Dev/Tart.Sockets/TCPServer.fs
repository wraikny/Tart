    namespace wraikny.Tart.Sockets.TCP

    open System
    open System.Linq
    open System.Threading
    open System.Net
    open System.Net.Sockets
    open System.Collections.Generic

    open wraikny.Tart.Helper.Utils
    open wraikny.Tart.Sockets


    [<AbstractClass>]
    type ServerBase<'SendMsg, 'RecvMsg>(encoder, decoder, bufferSize, endpoint) =
        let mutable nextClientID : ClientID = LanguagePrimitives.GenericZero

        let _lockObj = new Object()
        let clients = new Dictionary<ClientID, ClientBase<'SendMsg, 'RecvMsg>>()
        let endpoint = endpoint
        let mutable listener : Socket = null

        let encoder : 'SendMsg -> byte [] = encoder
        let decoder : byte[] -> 'RecvMsg option = decoder
        
        let bufferSize : int = bufferSize

        let createClient s = new ClientBase<_, _>(encoder, decoder, s, bufferSize, DebugDisplay = false)

        let receiveQueue = new MsgQueue<ClientID * 'RecvMsg>()
        let sendQueue = new MsgQueue<'SendMsg>()


        let mutable cancelAccepting = null
        let mutable cancelMessaging = null

        let mutable _debugDisplay = false

        new (encoder, decoder, bufferSize, port, ?ipAddress) =
            let ipAddress = defaultArg ipAddress IPAddress.Any
            new ServerBase<_, _>(encoder, decoder, bufferSize, IPEndPoint(ipAddress, port))


        member this.Clients
            with get() =
                lock _lockObj <| fun _ ->
                    clients
                        .Select(fun i -> (i.Key, i.Value :> IClientHandler<'SendMsg>))
                        .ToList()
                    :> IEnumerable<_>

        member this.TryGetClient(clientId) =
            lock _lockObj <| fun _ ->
                clients.TryGetValue(clientId)
                |> function
                | true, client -> client :> IClientHandler<'SendMsg> |> Some
                | _ -> None
        
        abstract OnConnectedClientAsync : ClientID -> unit
        default __.OnConnectedClientAsync _ = ()

        abstract OnPopReceiveMsgAsync : ClientID * 'RecvMsg -> Async<unit>
        abstract OnPopSendMsgAsync : 'SendMsg -> Async<unit>

        member __.DebugDisplay
            with get() = _debugDisplay
            and  set(value) = _debugDisplay <- value

        member inline private this.DebugPrint(s) =
            if this.DebugDisplay then
                Console.WriteLine(sprintf "[wraikny.Tart.Sockets.TCP.ServerBase] %A" s)


        member this.RemoveClient(clientId) =
            lock _lockObj <| fun _ ->
                if clients.ContainsKey(clientId) then
                    clients.Remove(clientId) |> ignore
                    this.DebugPrint(sprintf "Removed Client of %A" clientId)


        member private this.AsyncClientsDispatch() =
            let removeIds = new List<ClientID>()

            let rec loop() = async {
                let clientsList = lock _lockObj <| fun _ -> clients.ToList()
                for i in clientsList do
                    let client = i.Value
                    try
                        do! client.Dispatch()
                        if not client.IsConnected then
                            removeIds.Add(i.Key)
                    with
                    | :? SocketException ->
                        removeIds.Add(i.Key)

                lock _lockObj <| fun _ ->
                    for id in removeIds do
                        this.DebugPrint(sprintf "Remove client (id: %A)" id)
                        clients.Remove(id) |> ignore

                removeIds.Clear()

                Thread.Sleep(5)
                return! loop()
            }

            loop()

        member private this.AsyncPopReceiveMsg() =
            let rec loop() = async {
                match receiveQueue.TryDequeue() with
                | Some(msg) ->
                    do! this.OnPopReceiveMsgAsync(msg)
                | None -> ()
                return! loop()
            }

            loop()


        member private this.AsyncReceive() =
            async {
                let clientsList = lock _lockObj <| fun _ -> clients.ToList()

                for i in clientsList do
                    let client = i.Value
                    for r in client.Receive() do
                        this.DebugPrint(sprintf "Received message %A from %A" r i.Key)
                        (receiveQueue :> IMsgQueue<_>).Enqueue(i.Key,  r)
            }


        member private this.AsyncPopSendMsg() =
            let rec loop() = async {
                match sendQueue.TryDequeue() with
                | Some(msg) ->
                    this.DebugPrint(sprintf "Pop SendMsg: %A" msg)
                    do! this.OnPopSendMsgAsync(msg)
                    return! loop()
                | None -> ()
            }

            loop()


        member private this.AsyncServerDispatch() =
            async {
                while true do
                    do! this.AsyncPopSendMsg()
                    do! this.AsyncReceive()
                    // this.DebugPrint("Dispathing")
                    Thread.Sleep(5)
            }


        member this.IServer with get() = this :> IServer<_>


        interface IServer<'SendMsg> with
            member this.IsAccepting with get() = cancelAccepting <> null

            member this.IsMessaging with get() = cancelMessaging <> null
            
            member this.StartAcceptingAsync() =
                if this.IServer.IsAccepting then
                    raise <| InvalidOperationException()

                this.DebugPrint("Start Accepting")

                cancelAccepting <- new CancellationTokenSource()

                listener <- new Socket(AddressFamily.InterNetworkV6, SocketType.Stream, ProtocolType.Tcp)
                
                listener.Bind(endpoint)
                listener.Listen(int SocketOptionName.MaxConnections)

                this.DebugPrint(sprintf "Start Listening at %A" endpoint)

                let rec loop() = async {
                    let! socket = listener.AsyncAccept()
                
                    this.OnConnectedClientAsync(nextClientID)
                
                    lock _lockObj <| fun _ ->
                        clients.Add(nextClientID, createClient socket )

                        this.DebugPrint(sprintf "Accepted client (id: %A)" nextClientID)

                        nextClientID <- nextClientID + LanguagePrimitives.GenericOne
                
                    return! loop()
                }
                
                loop()
                |> fun a -> Async.Start(a, cancelAccepting.Token)

                // this.IServer


            member this.StopAcceptingAsync() =
                if not this.IServer.IsAccepting then
                    raise <| InvalidOperationException()

                this.DebugPrint("Stop Accepting")

                cancelAccepting.Cancel()

                listener.Close()
                listener <- null
                
                cancelAccepting <- null

                // this.IServer


            member this.StartMessaging() =
                if this.IServer.IsMessaging then
                    raise <| InvalidOperationException()

                this.DebugPrint("Start Messaging")

                cancelMessaging <- new CancellationTokenSource()

                [|
                    this.AsyncClientsDispatch()
                    this.AsyncServerDispatch()
                    this.AsyncPopReceiveMsg()
                |]
                |> Async.Parallel
                |> Async.Ignore
                |> fun a -> Async.Start(a, cancelMessaging.Token)

                // this.IServer


            member this.StopMessaging() =
                if not this.IServer.IsMessaging then
                    raise <| InvalidOperationException()

                this.DebugPrint("Stop Messaging")

                cancelMessaging.Cancel()

                cancelMessaging <- null

                // this.IServer


        interface IMsgQueue<'SendMsg> with
            member this.Enqueue(msg) =
                (sendQueue :> IMsgQueue<_>).Enqueue(msg)

        interface IDisposable with
            member this.Dispose() =
                this.DebugPrint("Dispose")
                for c in clients do
                    (c.Value :> IDisposable).Dispose()
                clients.Clear()

                this.DebugPrint("Clients Cleared")

                if this.IServer.IsAccepting then
                    this.IServer.StopAcceptingAsync() |> ignore

                if this.IServer.IsMessaging then
                    this.IServer.StopMessaging() |> ignore