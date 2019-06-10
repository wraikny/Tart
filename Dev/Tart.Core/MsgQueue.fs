namespace wraikny.Tart.Core

open System.Collections.Concurrent


[<Class>]
type MsgQueue<'Msg>() =
    let msgQueue = new ConcurrentQueue<'Msg>()

    member __.TryPopMsg() : 'Msg option =
        let success, result = msgQueue.TryDequeue()
        if success then
            Some result
        else
            None

    interface IMsgSender<'Msg> with
        member __.PushMsg(msg) = msgQueue.Enqueue(msg)