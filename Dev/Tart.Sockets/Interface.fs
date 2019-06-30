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

    abstract StartAccepting : unit -> IServer<'Msg>
    abstract StopAccepting : unit -> IServer<'Msg>

    abstract StartMessaging : unit -> IServer<'Msg>
    abstract StopMessaging : unit -> IServer<'Msg>

end


type IClient<'Msg> = interface
    inherit IDisposable
    inherit IMsgQueue<'Msg>

    abstract IsConnected : bool
end