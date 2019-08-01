namespace wraikny.Tart.Socket

open System
open System.Net
open System.Net.Sockets

open wraikny.Tart.Helper.Utils

// [<Struct>]
type ClientID = uint32

type MsgTarget =
    | Everyone
    | Client of ClientID
    | Clients of ClientID list


type IClientHandler<'Msg> = interface
    inherit IDisposable
    inherit IEnqueue<'Msg>

    abstract IsConnected : bool with get

    abstract SendMsgAsync : 'Msg -> Async<unit>
    // abstract SendSync : 'Msg -> unit
end


type IServer<'SendMsg, 'RecvMsg> = interface
    inherit IDisposable
    inherit IEnqueue<MsgTarget * 'SendMsg>

    abstract IsAccepting : bool with get
    abstract IsMessaging : bool with get

    abstract StartAcceptingAsync : unit -> unit
    abstract StopAccepting : unit -> unit

    abstract StartMessagingAsync : unit -> unit
    abstract StopMessaging : unit -> unit

    abstract OnClientConnected : IEvent<ClientID * IClientHandler<'SendMsg>> with get
    abstract OnClientDisconnected : IEvent<ClientID> with get
    abstract OnReceiveMsg : IEvent<ClientID * 'RecvMsg> with get
    abstract OnError : IEvent<exn> with get
end


type IClient<'SendMsg, 'RecvMsg> = interface
    inherit IDisposable
    inherit IEnqueue<'SendMsg>

    abstract ClientId : ClientID with get

    abstract IsConnected : bool with get

    abstract StartAsync : IPEndPoint -> unit

    abstract OnReceiveMsg : IEvent<'RecvMsg> with get
    abstract OnDisconnected : IEvent<unit> with get
    abstract OnError : IEvent<exn> with get

    // abstract Send : 'Msg -> Async<unit>
end

open FSharpPlus


[<Struct>]
type internal SocketMsg<'Msg> =
    // 0
    | UserMsg of msg:'Msg

with
    member msg.Encode(encoder) =
        let flag, bytes = 
            msg |> function
            | UserMsg msg ->
                0uy, (encoder msg)

        [|flag|] <|> bytes

    static member Decode(decoder, decrypted) =
        let flag, bytes = Array.splitAt 1 decrypted
        monad {
            let! head = flag |> tryHead
            match head with
            | 0uy ->
                let! msg = decoder bytes
                return UserMsg msg
            | _ ->
                return! None
        }