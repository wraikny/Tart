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



[<AbstractClass>]
type ServerBase<'SendMsg, 'RecvMsg>(encoder, decoder, endpoint) =
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

    new (encoder, decoder, port, ?ipAddress) =
        let ipAddress = defaultArg ipAddress IPAddress.Any
        new ServerBase<_, _>(encoder, decoder, IPEndPoint(ipAddress, port))

    // member val MaxClients : uint16 option = None with get, set


    abstract OnPopRecvMsg : ClientID * 'RecvMsg -> Async<unit>
    abstract OnPopSendMsg : MsgTarget * 'SendMsg -> Async<unit>

    abstract OnConnected : ClientID * IClientHandler<'SendMsg> -> unit
    default __.OnConnected (_, _) = ()

    abstract OnDisconnected : ClientID -> unit
    default __.OnDisconnected (_) = ()

    abstract OnFailedToSend : ClientID * IClientHandler<'SendMsg> * 'SendMsg -> Async<unit>
    default __.OnFailedToSend(_, _, _) = async{ () }

    abstract OnFailedToReceive : ClientID * IClientHandler<'SendMsg> -> Async<unit>
    default __.OnFailedToReceive(_, _) = async{ () }
    

    member private server.CreateClient(s, clientId) =
        { new ClientBase<_, _>(encoder, decoder, s, DebugDisplay = false) with

            override __.OnDisconnected() = server.OnDisconnected(clientId)

            override client.OnFailedToSend(msg) =
                server.OnFailedToSend(clientId, client :> IClientHandler<_>, msg)

            override client.OnFailedToReceive() =
                server.OnFailedToReceive(clientId, client :> IClientHandler<_>)
        }


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

    member inline private this.DebugPrint(s) =
        if this.DebugDisplay then
            Console.WriteLine(sprintf "[wraikny.Tart.Sockets.TCP.ServerBase] %A" s)


    member this.RemoveClient(clientId) =
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
                do! this.OnPopRecvMsg(msg)
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
                    receiveQueue.Enqueue(i.Key,  r)
        }


    member private this.AsyncPopSendMsg() =
        let rec loop() = async {
            match sendQueue.TryDequeue() with
            | Some(msg) ->
                this.DebugPrint(sprintf "Pop SendMsg: %A" msg)
                do! this.OnPopSendMsg(msg)
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


    member private this.IServer with get() = this :> IServer<_>


    interface IServer<'SendMsg> with
        member this.IsAccepting with get() = not (isNull cancelAccepting)

        member this.IsMessaging with get() = not (isNull cancelMessaging)
            
        member this.StartAcceptingAsync() =
            if this.IServer.IsAccepting then
                raise <| InvalidOperationException()

            this.DebugPrint("Start Accepting")


            listener <- new Socket(AddressFamily.InterNetworkV6, SocketType.Stream, ProtocolType.Tcp)
            
            listener.Bind(endpoint)
            listener.Listen(int SocketOptionName.MaxConnections)

            this.DebugPrint(sprintf "Start Listening at %A" endpoint)

            let rec loop() = async {
                //match this.MaxClients with
                //| Some(x) when x <= (lock _lockObj <| fun _ -> clientsCount) ->
                //    this.DebugPrint("Clients Count is over MaxClients.")
                //    Thread.Sleep(1000)
                //    return! loop()
                //| _ ->

                let! socket = listener.AsyncAccept()
                let client = this.CreateClient(socket, nextClientID)
                do! client.InitCryptOfServer()

                do! client.EncryptedSendWithHead(BitConverter.GetBytes(nextClientID)) |> Async.Ignore

                lock _lockObj <| fun _ ->
                    clients.Add(nextClientID, client )
                    // clientsCount <- clientsCount + 1us

                    this.DebugPrint(sprintf "Accepted client (id: %A)" nextClientID)

                    nextClientID <- nextClientID + LanguagePrimitives.GenericOne

                this.OnConnected(nextClientID, client)
                
                return! loop()
            }
                
            cancelAccepting <- new CancellationTokenSource()

            loop()
            |> fun a -> Async.Start(a, cancelAccepting.Token)


        member this.StopAccepting() =
            if not this.IServer.IsAccepting then
                raise <| InvalidOperationException()

            this.DebugPrint("Stopping Accepting")

            cancelAccepting.Cancel()
            cancelAccepting <- null

            listener.Close()
            listener <- null
            
            this.DebugPrint("Stopped Accepting")


        member this.StartMessagingAsync() =
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


        member this.StopMessaging() =
            if not this.IServer.IsMessaging then
                raise <| InvalidOperationException()

            this.DebugPrint("Stopping Messaging")

            cancelMessaging.Cancel()
            cancelMessaging <- null

            this.DebugPrint("Stopped Messaging")


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


