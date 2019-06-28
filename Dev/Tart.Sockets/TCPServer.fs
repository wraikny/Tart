namespace wraikny.Tart.Sockets

open System
open System.Threading
open System.Net
open System.Net.Sockets
open System.Collections.Generic

open wraikny.Tart.Helper.Utils

// http://www.fssnip.net/1E/title/Async-TCP-Server
[<AbstractClass>]
type TCPServerBase private (endpoint, displayLog) =
    
    let mutable nextClientID = LanguagePrimitives.GenericZero
    let clients = new Dictionary<ClientID, Socket>()

    let endpoint = endpoint
    let displayLog = defaultArg displayLog true
    

    new(port, ?ipAddress, ?displayLog) =
        let ipAddress = defaultArg ipAddress IPAddress.Any
        new TCPServerBase(IPEndPoint(ipAddress, port), displayLog)


    member this.WaitClientsConnection() =
        let listener = new Socket(AddressFamily.InterNetwork, SocketType.Stream, ProtocolType.Tcp)
        listener.Bind(endpoint)
        listener.Listen(int SocketOptionName.MaxConnections)

        let rec loop() = async {
            let! client = listener.AsyncAccept()

            lock clients <| fun _ ->
                clients.Add(nextClientID, client)
                nextClientID <- nextClientID + LanguagePrimitives.GenericOne

            return! loop()
        }

        loop()

        //let cts = new CancellationTokenSource()
        //Async.Start(loop(), cancellationToken = cts.Token)
        //{ new IDisposable with member x.Dispose() = cts.Cancel(); listener.Close() }