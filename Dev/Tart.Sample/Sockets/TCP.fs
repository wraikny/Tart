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
let decoder (bytes : byte[]) = Some <| encoding.GetString(bytes)

let bufferSize = 1024

type SMsg = string
type CMsg = string

type TestServer(ipEndpoint) =
    inherit ServerBase<SMsg, CMsg>(encoder, decoder, bufferSize, ipEndpoint)

    let print s = ()

    override this.OnPopReceiveMsgAsync (clientId, recvMsg) =
        async {
            if recvMsg = "!remove" then
                this.Clients.Remove(clientId) |> ignore
            print <| sprintf "received %A from (id: %A)" recvMsg clientId
        }

    override this.OnPopSendMsgAsync(sendMsg) =
        async {
            for c in this.Clients do
                let client = c.Value
                (client :> IMsgQueue<_>).Enqueue(sendMsg)
            print <| sprintf "send %A" sendMsg
        }


type TestClient() =
    inherit Client<CMsg, SMsg>(encoder, decoder, bufferSize)

    override this.OnPopRecvMsg(msg) = ()


let waiting() =
    Thread.Sleep(100)
    StaticLock.Printfn "Enter.."
    Console.ReadLine() |> ignore


let main _ =
    let ipEndpoint =
        let ipAdd = Dns.GetHostEntry("localhost").AddressList.[0]
        IPEndPoint(ipAdd, 8000)

    use server = new TestServer(ipEndpoint)
    server.DebugDisplay <- true

    use client = new Client<string, string>(encoder, decoder, 1024)
    client.DebugDisplay <- true

    waiting()

    server.IServer
        .StartAccepting()
        .StartMessaging()
        |> ignore

    waiting()

    client.AsyncStart(ipEndpoint)

    waiting()


    (server :> IMsgQueue<_>).Enqueue("Hello! from Server")
    StaticLock.Printfn "Server Enqueued"

    waiting()

    (client :> IMsgQueue<_>).Enqueue("Hello! from client")
    StaticLock.Printfn "Client Enqueued"

    waiting()

    (client :> IMsgQueue<_>).Enqueue("!remove")

    //waiting()
    client.Disconnect()

    // ここで死ぬ

    waiting()

    (server :> IMsgQueue<_>).Enqueue("Hello! from Server")
    StaticLock.Printfn "Server Enqueued"

    waiting()

    //StaticLock.Printfn(sprintf "Connected Clients Count: %d" server.Clients.Count)
    //server.Disconnect()
    //StaticLock.Printfn(sprintf "Connected Clients Count: %d" server.Clients.Count)

    waiting()

    ()