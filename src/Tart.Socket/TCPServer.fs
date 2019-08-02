namespace wraikny.Tart.Socket.TCP

open System
open System.Linq
open System.Threading
open System.Net
open System.Net.Sockets
open System.Collections.Generic

open wraikny.Tart.Helper.Utils
open wraikny.Tart.Socket


open System.Security.Cryptography
open System.Text



type Server<'SendMsg, 'RecvMsg>(encoder, decoder, endpoint) =
    let mutable nextClientID : ClientID = LanguagePrimitives.GenericZero

    let _lockObj = Object()
    let clients = new Dictionary<ClientID, ClientBase<'SendMsg, 'RecvMsg>>()
    // let mutable clientsCount = 0us

    let endpoint = endpoint
    let mutable listener : Socket = null

    let encoder : 'SendMsg -> byte [] = encoder
    let decoder : byte[] -> 'RecvMsg option = decoder

    let receiveQueue = new MsgQueue<ClientID * 'RecvMsg>() :> IQueue<_>
    let sendQueue = new MsgQueue<MsgTarget * 'SendMsg>() :> IQueue<_>


    let mutable cancelAccepting = null
    let mutable cancelMessaging = null

    let mutable _debugDisplay = false

    let onClientConnectedEvent = new Event<ClientID * IClientHandler<'SendMsg>>()
    let onClientDisconenctedEvent = new Event<ClientID>()
    let onReceiveMsgEvent = new Event<ClientID * 'RecvMsg>()
    let onErrorEvent = new Event<exn>()

    new (encoder, decoder, port, ?ipAddress) =
        let ipAddress = defaultArg ipAddress IPAddress.Any
        new Server<_, _>(encoder, decoder, IPEndPoint(ipAddress, port))

    // member val MaxClients : uint16 option = None with get, set
    

    member private server.CreateClient(s, clientId) =
        let client = new ClientBase<_, _>(encoder, decoder, s, DebugDisplay = false)
        client.OnDisconnected.Add(fun () ->
            server.DebugPrint(sprintf "Disconnected of %d" clientId)

            onClientDisconenctedEvent.Trigger(clientId)

            server.RemoveClient(clientId) |> ignore
        )
        client.OnReceiveMsg.Add(fun r ->
            server.DebugPrint(sprintf "Received message %A from %A" r clientId)
            receiveQueue.Enqueue(clientId,  r)
        )
        client.OnError.Add(fun e ->
            Console.WriteLine("Error occured in client od {0}", clientId)
            server.DebugPrint(string e)
            onErrorEvent.Trigger(e)
            server.RemoveClient(clientId) |> ignore
        )
        client



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
       

    member __.DebugDisplay
        with get() = _debugDisplay
        and  set(value) = _debugDisplay <- value

    member private this.DebugPrint(s) =
        if this.DebugDisplay then
            Console.WriteLine(sprintf "[wraikny.Tart.Sockets.TCP.ServerBase] %A" s)


    member this.RemoveClient(clientId) : bool =
        lock _lockObj <| fun _ ->
            clients.TryGetValue(clientId) |> function
            | true, client ->
                (client :> IDisposable).Dispose()
                clients.Remove(clientId) |> ignore
                // clientsCount <- clientsCount - 1us
                this.DebugPrint(sprintf "Removed Client of %A" clientId)
                true
            | _ -> false


    member this.SendTo(id, msg) =
        (lock _lockObj <| fun _ -> clients.TryGetValue(id))
        |> function
        | true, client ->
            (client :> IEnqueue<_>).Enqueue(msg)
            true
        | _ ->
            false

    member this.SendToEveryone(msg) =
        for (_, client) in this.Clients do
            client.Enqueue(msg)


    member this.SendToOthers(nonTargetId, msg) =
        let mutable t = true
        for (clientId, client) in this.Clients do
            if t && clientId = nonTargetId then
                t <- false
            else
                client.Enqueue(msg)


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
                    // clientsCount <- clientsCount - 1us

            removeIds.Clear()

            Thread.Sleep(5)
            return! loop()
        }

        loop()

    member private this.AsyncPopReceiveMsg() =
        let rec loop() = async {
            match receiveQueue.TryDequeue() with
            | Some(msg) ->
                onReceiveMsgEvent.Trigger(msg)
            | None -> ()
            return! loop()
        }

        loop()


    member private this.AsyncReceive() =
        async {
            let clientsList = lock _lockObj <| fun _ -> clients.ToList()

            for i in clientsList do
                do! i.Value.Receive()
        }


    member private this.AsyncPopSendMsg() =
        let rec loop() = async {
            match sendQueue.TryDequeue() with
            | Some(target, msg) ->
                this.DebugPrint(sprintf "Pop SendMsg To %A: %A" target msg)
                target |> function
                | Everyone -> ()
                | Client(clientId) ->
                    this.TryGetClient(clientId)
                    |> Option.iter(fun c -> c.Enqueue(msg))
                | Clients(clientIds) ->
                    for clientId in clientIds do
                        this.TryGetClient(clientId)
                        |> Option.iter(fun c -> c.Enqueue(msg))
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


    member private this.IServer with get() = this :> IServer<_, _>


    interface IServer<'SendMsg, 'RecvMsg> with
        member this.IsAccepting with get() = not (isNull cancelAccepting)

        member this.IsMessaging with get() = not (isNull cancelMessaging)
            
        member this.StartAcceptingAsync() =
            if this.IServer.IsAccepting then
                raise <| InvalidOperationException()

            this.DebugPrint("Start Accepting")

            try
            listener <- new Socket(AddressFamily.InterNetworkV6, SocketType.Stream, ProtocolType.Tcp)
            
            listener.Bind(endpoint)
            listener.Listen(int SocketOptionName.MaxConnections)

            this.DebugPrint(sprintf "Start Listening at %A" endpoint)

            let rec loop() = async {
                try
                    this.DebugPrint("Accepting..")
                    //match this.MaxClients with
                    //| Some(x) when x <= (lock _lockObj <| fun _ -> clientsCount) ->
                    //    this.DebugPrint("Clients Count is over MaxClients.")
                    //    Thread.Sleep(1000)
                    //    return! loop()
                    //| _ ->
                    let clientId = nextClientID

                    let! socket = listener.AsyncAccept()
                    
                    try
                        let client = this.CreateClient(socket, clientId)

                        do! client.InitCryptOfServer()

                        do! client.EncryptedSendWithHead(BitConverter.GetBytes(clientId)) |> Async.Ignore

                        lock _lockObj <| fun _ ->
                            clients.Add(clientId, client )
                            // clientsCount <- clientsCount + 1us

                            this.DebugPrint(sprintf "Accepted client (id: %A)" clientId)

                            nextClientID <- nextClientID + LanguagePrimitives.GenericOne

                        onClientConnectedEvent.Trigger(clientId, client :> IClientHandler<_>)
                    with e ->
                        this.DebugPrint(sprintf "Client of %d failed encrypting: %A" clientId e)
                        onErrorEvent.Trigger(e)
                
                    return! loop()
                with e ->
                    this.DebugPrint(string e)
                    onErrorEvent.Trigger(e)
            }
                
            cancelAccepting <- new CancellationTokenSource()

            loop()
            |> fun a -> Async.Start(a, cancelAccepting.Token)
            with e ->
                this.DebugPrint(e.ToString())
                onErrorEvent.Trigger(e)


        member this.StopAccepting() =
            if not this.IServer.IsAccepting then
                raise <| InvalidOperationException()

            this.DebugPrint("Stopping Accepting")
            try
                cancelAccepting.Cancel()
                cancelAccepting <- null

                listener.Close()
                listener <- null
            with e ->
                this.DebugPrint(e.ToString())
                onErrorEvent.Trigger(e)
            
            this.DebugPrint("Stopped Accepting")


        member this.StartMessagingAsync() =
            if this.IServer.IsMessaging then
                raise <| InvalidOperationException()

            this.DebugPrint("Start Messaging")

            cancelMessaging <- new CancellationTokenSource()
            async {
                try
                    return!
                        [|
                            this.AsyncClientsDispatch()
                            this.AsyncServerDispatch()
                            this.AsyncPopReceiveMsg()
                        |]
                        |> Async.Parallel
                        |> Async.Ignore
                with e ->
                    this.DebugPrint(string e)
                    onErrorEvent.Trigger(e)
            }
            |> fun a -> Async.Start(a, cancelMessaging.Token)


        member this.StopMessaging() =
            if not this.IServer.IsMessaging then
                raise <| InvalidOperationException()

            this.DebugPrint("Stopping Messaging")

            cancelMessaging.Cancel()
            cancelMessaging <- null

            this.DebugPrint("Stopped Messaging")

        member __.OnClientConnected with get() = onClientConnectedEvent.Publish
        member __.OnClientDisconnected with get() = onClientDisconenctedEvent.Publish

        member __.OnReceiveMsg with get() = onReceiveMsgEvent.Publish
        member __.OnError with get() = onErrorEvent.Publish


    interface IEnqueue<MsgTarget * 'SendMsg> with
        member this.Enqueue(msg) = sendQueue.Enqueue(msg)

    interface IDisposable with
        member this.Dispose() =
            this.DebugPrint("Dispose")
            if this.IServer.IsAccepting then
                this.IServer.StopAccepting() |> ignore

            if this.IServer.IsMessaging then
                this.IServer.StopMessaging() |> ignore

            lock _lockObj <| fun _ ->
                for c in clients do
                    (c.Value :> IDisposable).Dispose()
                clients.Clear()
                // clientsCount <- 0us

            this.DebugPrint("Clients Cleared")


