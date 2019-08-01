namespace wraikny.Tart.Socket

open System
open System.Net
open System.Net.Sockets

open wraikny.Tart.Helper.Utils

module TCPServer =
    let inline create< ^SendMsg, ^RecvMsg
        when ^SendMsg : (member Encode : unit -> byte [])
        and  ^RecvMsg : (static member Decode : byte [] -> ^RecvMsg option)
        > ipEndPoint debugDisplay =
        new TCP.Server< ^SendMsg, ^RecvMsg>(
            Bytes.encode, Bytes.decode, ipEndPoint,
            DebugDisplay = debugDisplay )
        :> IServer<_, _>

module TCPClient =
    let inline create< ^SendMsg, ^RecvMsg
        when ^SendMsg : (member Encode : unit -> byte [])
        and  ^RecvMsg : (static member Decode : byte [] -> ^RecvMsg option)
        > (debugDisplay) =
        new TCP.Client< ^SendMsg, ^RecvMsg>(
            Bytes.encode, Bytes.decode,
            DebugDisplay = debugDisplay )
        :> IClient<_, _>