namespace wraikny.Tart.Core

/// Telling msg
type IMsgSender<'Msg> =
    /// Add Msg
    abstract PushMsg : 'Msg -> unit


open System.Collections.Concurrent


[<Class>]
type MsgQueue<'Msg>() =
    let msgQueue = new ConcurrentQueue<'Msg>()

    member __.TryPopMsg() : 'Msg option =
        msgQueue.TryDequeue() |> function
        | true, result -> Some result
        | false, _ -> None

    interface IMsgSender<'Msg> with
        member __.PushMsg(msg) = msgQueue.Enqueue(msg)