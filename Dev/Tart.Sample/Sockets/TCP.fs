module wraikny.Tart.Sample.Sockets.TCP

open System
open System.Linq
open System.Threading
open System.Net
open System.Net.Sockets
open System.Collections.Generic

open wraikny.Tart.Helper.Utils
open wraikny.Tart.Sockets
open wraikny.Tart.Sockets.TCP


let encoding = System.Text.Encoding.UTF8


let encoder (s : string) = encoding.GetBytes(s)
let decoder (bytes : byte[]) = encoding.GetString(bytes)

[<Literal>]
let iv = "aaaaaaaaaaaaaaaa"

[<Literal>]
let key = "ssssssssssssssssssssssssssssssss"

type SMsg = SMsg of string

module SMsg =
    let value = function SMsg s -> s

    let encoder = value >> encoder

    let decoder = decoder >> SMsg >> Some


type SMsg with
    member inline x.Value with get() = x |> SMsg.value

type CMsg = CMsg of string

module CMsg =
    let value = function CMsg s -> s

    let encoder = value >> encoder
    
    let decoder = decoder >> CMsg >> Some

type CMsg with
    member inline x.Value with get() = x |> CMsg.value

type TestServer(ipEndpoint : IPEndPoint) =
    inherit ServerBase<SMsg, CMsg>(iv, key, SMsg.encoder, CMsg.decoder, ipEndpoint)

    override this.OnPopReceiveMsgAsync (clientId, recvMsg) =
        async {
            if recvMsg.Value = "!remove" then
                let client = this.TryGetClient(clientId).Value
                this.RemoveClient(clientId)
            Console.WriteLine(sprintf "Received %s from (id: %A)" recvMsg.Value clientId)
        }

    override this.OnPopSendMsgAsync(sendMsg) =
        async {
            for (_, client) in this.Clients do
                client.Enqueue(sendMsg)
        }

    override this.OnClientFailedToSend(_, _, _) = ()
    override this.OnClientFailedToReceive(_, _) = ()


type TestClient() =
    inherit Client<CMsg, SMsg>(iv, key, CMsg.encoder, SMsg.decoder)

    override this.OnPopRecvMsg(msg) =
        msg.Value |> function
        | "!remove" -> (this :> IClient<_>).Dispose()
        | _ -> ()
        ()

    override this.OnConnecting() = ()
    override this.OnConnected() = ()
    override this.OnDisconnected() = ()
    override this.OnFailedToSend _ = ()
    override this.OnFailedToReceive() = ()


let waiting() =
    Thread.Sleep(100)
    Console.WriteLine("Enter..")
    Console.ReadLine() |> ignore

let main() =
    let ipEndpoint =
        let ipAdd = Dns.GetHostEntry("localhost").AddressList.[0]
        IPEndPoint(ipAdd, 8000)

    let server = new TestServer(ipEndpoint, DebugDisplay = true) :> IServer<_>

    let client = new TestClient(DebugDisplay = true) :> IClient<_>

    server.StartAcceptingAsync()
    server.StartMessagingAsync()

    client.StartAsync(ipEndpoint)

    while not client.IsConnected do
        Console.WriteLine("waiting connection ...")
        Thread.Sleep(5)

    waiting()

    server.Enqueue(SMsg "Hello! from Server")
    client.Enqueue(CMsg "Hello! from client")

    waiting()


    // 終了処理
    client.Dispose()

    waiting()

    server.Dispose()

    waiting()


let main'() =
    let ipEndpoint =
        let ipAdd = Dns.GetHostEntry("localhost").AddressList.[0]
        IPEndPoint(ipAdd, 8000)

    let server = new TestServer(ipEndpoint, DebugDisplay = true) :> IServer<_>

    server.StartAcceptingAsync()
    server.StartMessagingAsync()

    waiting()

    let clients = [|for _ in 1..10 -> new TestClient(DebugDisplay = true) :> IClient<_>|]

    clients |> Seq.iter(fun c -> c.StartAsync(ipEndpoint))

    waiting()

    clients |> Seq.indexed |> Seq.iter(fun (i, c) -> c.Enqueue(CMsg <| sprintf "Hello from Client %d" i))

    waiting()

    for _ in 1..5 do
        server.Enqueue(SMsg "Hello from server")

    waiting()


    clients |> Seq.iter(fun c -> c.Dispose())

    waiting()

    server.Dispose()

    waiting()
