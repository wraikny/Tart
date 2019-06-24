namespace wraikny.Tart.Helper.Utils

/// Telling msg
type IMsgQueue<'T> = interface
    /// Add Msg
    abstract Enqueue : 'T -> unit
end


open System.Collections.Concurrent


type MsgQueue<'T>() = class
    let queue = new ConcurrentQueue<'T>()

    member __.TryPopMsg() : 'T option =
        queue.TryDequeue() |> function
        | true, result -> Some result
        | false, _ -> None

    interface IMsgQueue<'T> with
        member __.Enqueue(msg) = queue.Enqueue(msg)
end