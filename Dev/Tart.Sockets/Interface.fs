namespace wraikny.Tart.Sockets

open System
open System.Net
open System.Net.Sockets

open wraikny.Tart.Helper.Utils

// [<Struct>]
type ClientID = uint32


type IServer<'SendMsg, 'RecvMsg> = interface
    inherit IDisposable

    abstract Start : unit -> unit

    abstract IsRunning : bool with get
    abstract SleepTime : uint32 with get
end


type IClient<'SendMsg, 'RecvMsg> = interface
    inherit IDisposable

    abstract Connect : IPEndPoint -> unit
    abstract Connect : IPEndPoint * int -> unit

    abstract Start : unit -> unit
    abstract Stop : unit -> unit

    abstract IsRunning : bool with get
    abstract SleepTime : uint32 with get
end