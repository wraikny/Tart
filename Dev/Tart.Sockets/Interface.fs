namespace wraikny.Tart.Sockets

open System
open System.Net
open System.Net.Sockets

open wraikny.Tart.Helper.Utils

// [<Struct>]
type ClientID = uint32


type IServer<'Msg> = interface
    inherit IDisposable
    inherit IMsgQueue<'Msg>

    abstract IsAccepting : bool with get
    abstract IsMessaging : bool with get

    abstract StartAcceptingAsync : unit -> unit
    abstract StopAccepting : unit -> unit

    abstract StartMessagingAsync : unit -> unit
    abstract StopMessaging : unit -> unit
end

type IClientHandler<'Msg> = interface
    inherit IDisposable
    inherit IMsgQueue<'Msg>

    abstract IsConnected : bool with get
    // abstract SendSync : 'Msg -> unit
end


type IClient<'Msg> = interface
    inherit IDisposable
    inherit IMsgQueue<'Msg>

    abstract IsConnected : bool with get

    abstract StartAsync : IPEndPoint -> unit

    // abstract Send : 'Msg -> Async<unit>
end


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

        Array.append [|flag|] bytes

    static member Decode(decoder, decrypted) =
        let flag, bytes = Array.splitAt 1 decrypted
        flag.[0] |> function
        | 0uy ->
            decoder bytes |> Option.map(UserMsg)
        | _ -> None