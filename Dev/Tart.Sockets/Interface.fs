namespace wraikny.Tart.Sockets

open System
open System.Net
open System.Net.Sockets

open wraikny.Tart.Helper.Utils

// [<Struct>]
type ClientID = uint32


type SendMsg =
    | ToEveryone
    | ToOthers



type IServer<'Msg> = interface
    inherit IDisposable
    inherit IMsgQueue<'Msg>

    abstract IsAccepting : bool with get
    abstract IsMessaging : bool with get

    abstract StartAcceptingAsync : unit -> unit
    abstract StopAcceptingAsync : unit -> unit

    abstract StartMessaging : unit -> unit
    abstract StopMessaging : unit -> unit
end

type IClientHandler<'Msg> = interface
    inherit IDisposable
    inherit IMsgQueue<'Msg>

    abstract IsConnected : bool with get
    abstract SendSync : 'Msg -> unit
end


type IClient<'Msg> = interface
    inherit IDisposable
    inherit IMsgQueue<'Msg>

    abstract IsConnected : bool with get

    abstract StartAsync : IPEndPoint -> unit

    // abstract Send : 'Msg -> Async<unit>
end