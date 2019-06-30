﻿module wraikny.Tart.Sample.Sockets.TCP

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

let bufferSize = 1024

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

type TestServer(ipEndpoint) =
    inherit ServerBase<SMsg, CMsg>(SMsg.encoder, CMsg.decoder, bufferSize, ipEndpoint)

    override this.OnPopReceiveMsgAsync (clientId, recvMsg) =
        async {
            if recvMsg.Value = "!remove" then
                let client = this.TryGetClient(clientId).Value
                client.SendSync(SMsg "!remove")
                this.RemoveClient(clientId)
            Console.WriteLine(sprintf "Received %s from (id: %A)" recvMsg.Value clientId)
        }

    override this.OnPopSendMsgAsync(sendMsg) =
        async {
            for (_, client) in this.Clients do
                client.Enqueue(sendMsg)
        }


type TestClient() =
    inherit Client<CMsg, SMsg>(CMsg.encoder, SMsg.decoder, bufferSize)

    override this.OnPopRecvMsg(msg) =
        msg.Value |> function
        | "!remove" -> (this :> IClient<_>).Dispose()
        | _ -> ()
        ()

    override this.OnConnecting() = ()
    override this.OnConnected() = ()
    override this.OnDisconnected() = ()


let waiting() =
    Thread.Sleep(100)
    Console.WriteLine("Enter..")
    Console.ReadLine() |> ignore

let main _ =
    let ipEndpoint =
        let ipAdd = Dns.GetHostEntry("localhost").AddressList.[0]
        IPEndPoint(ipAdd, 8000)

    let server = new TestServer(ipEndpoint, DebugDisplay = true) :> IServer<_>

    let client = new TestClient(DebugDisplay = true) :> IClient<_>

    server.StartAcceptingAsync()
    server.StartMessaging()

    client.StartAsync(ipEndpoint)

    while not client.IsConnected do
        Console.WriteLine("waiting connection ...")
        Thread.Sleep(5)

    Thread.Sleep(100)

    server.Enqueue(SMsg "Hello! from Server")
    client.Enqueue(CMsg "Hello! from client")

    Thread.Sleep(100)
    Console.WriteLine("Enter..")
    Console.ReadLine() |> ignore

    client.Dispose()

    Console.WriteLine("Enter..")
    Console.ReadLine() |> ignore

    server.Dispose()

    Console.WriteLine("Enter..")
    Console.ReadLine() |> ignore