[<AutoOpen>]
module wraikny.Tart.Sockets.Extension

open System.Net.Sockets

// http://www.fssnip.net/1E/title/Async-TCP-Server
type Socket with
    member socket.AsyncAccept() =
        Async.FromBeginEnd(socket.BeginAccept, socket.EndAccept)

    member socket.AsyncConnect(remoteEndPoint) =
        let beginConnect(ep, cb, s) =
            socket.BeginConnect(ep, cb, s)

        Async.FromBeginEnd(remoteEndPoint, beginConnect, socket.EndConnect)

    member socket.AsyncDisconnect(reuseSocket) =
        let beginDisconnect(t, cb, s)=
            socket.BeginDisconnect(t, cb, s)

        Async.FromBeginEnd(reuseSocket, beginDisconnect, socket.EndDisconnect)

    member socket.AsyncReceive(buffer : byte[], ?offset, ?count) =
        let offset = defaultArg offset 0
        let count = defaultArg count buffer.Length

        let beginReceive(b, o, c, cb, s) =
            socket.BeginReceive(b, o, c, SocketFlags.None, cb, s)

        Async.FromBeginEnd(buffer, offset, count, beginReceive, socket.EndReceive)

    member socket.AsyncSend(buffer : byte[], ?offset, ?count) =
        let offset = defaultArg offset 0
        let count = defaultArg count buffer.Length

        let beginSend(b, o, c, cb, s) =
            socket.BeginSend(b, o, c, SocketFlags.None, cb, s)

        Async.FromBeginEnd(buffer, offset, count, beginSend, socket.EndSend)