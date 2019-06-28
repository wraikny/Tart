namespace wraikny.Tart.Sockets.TCP

open System
open System.Threading
open System.Net
open System.Net.Sockets
open System.Collections.Generic

open wraikny.Tart.Helper.Utils
open wraikny.Tart.Sockets


type ClientHandlerInServer(socket, bufferSize) =
    let socket : Socket = socket

    let bufferSize = bufferSize

    let sendQueue = new MsgQueue<byte []>()
    let recvQueue = new MsgQueue<byte []>()


    member this.Send(bytes) =
        (sendQueue :> IMsgQueue<_>).Enqueue(bytes)


    member this.Receive() =
        recvQueue.TryPopMsg()


    member this.DispatchSend() =
        if socket.Poll(0, SelectMode.SelectWrite) then
            let rec loop() =
                sendQueue.TryPopMsg() |> function
                | Some bytes ->
                    socket.Send(bytes, SocketFlags.None) |> ignore
                    loop()
                | None -> ()
            loop()


    member this.DispatchRecv() =
        if socket.Poll(0, SelectMode.SelectRead) then
            let buffer = [|for _ in 1..bufferSize -> 0uy|]
            let recvSize = socket.Receive(buffer, buffer.Length, SocketFlags.None)
            if recvSize = 0 then
                ()
            elif recvSize > 0 then
                (recvQueue :> IMsgQueue<_>).Enqueue(buffer)


    interface IDisposable with
        member this.Dispose() =
            socket.Shutdown(SocketShutdown.Both)
            socket.Close()



[<AbstractClass>]
type ServerBase<'SendMsg, 'RecvMsg>(encoder, decoder, bufferSize, endpoint) =
    let mutable nextClientID : ClientID = LanguagePrimitives.GenericZero

    let _lockObj = new Object()
    let clients = new Dictionary<ClientID, ClientHandlerInServer>()
    let endpoint = endpoint
    let listener : Socket = null

    let encoder : 'SendMsg -> byte[] = encoder
    let decoder : byte[] -> 'RecvMsg option = decoder

    let bufferSize : int = bufferSize

    new (encoder, decoder, bufferSize, port, ?ipAddress) =
        let ipAddress = defaultArg ipAddress IPAddress.Any
        new ServerBase<_, _>(encoder, decoder, bufferSize, IPEndPoint(ipAddress, port))


    member internal this.Clients with get() = clients


    member private this.AsyncWaitingClients() =
        if listener = null then raise <| InvalidOperationException()

        listener.Bind(endpoint)
        listener.Listen(int SocketOptionName.MaxConnections)

        let rec loop() = async {
            let! client = listener.AsyncAccept()

            lock _lockObj <| fun _ ->
                clients.Add(nextClientID, new ClientHandlerInServer(client, bufferSize) )
                nextClientID <- nextClientID + LanguagePrimitives.GenericOne

            return! loop()
        }

        loop()


    member private this.SendTo(id, msg : 'SendMsg) =
        let bytes = encoder msg
        let client = clients.[id]
        try
            client.Send(bytes) |> ignore
        with
            | :? SocketException ->
                clients.Remove(id) |> ignore

    
    abstract OnReceiveMsg : 'RecvMsg -> unit

    interface IDisposable with
        member this.Dispose() =
            for c in clients do
                (c.Value :> IDisposable).Dispose()



    member inline this.Start() = ()