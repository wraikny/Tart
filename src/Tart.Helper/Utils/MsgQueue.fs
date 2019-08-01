namespace wraikny.Tart.Helper.Utils
    
open System.Collections.Generic
open System.Collections.Concurrent


type MsgQueue<'T>() =
    let queue = new ConcurrentQueue<'T>()

    interface IQueue<'T> with
        member __.TryDequeue() : 'T option =
            queue.TryDequeue() |> function
            | true, result -> Some result
            | false, _ -> None

        member __.Enqueue(msg) = queue.Enqueue(msg)

        member __.Count with get() = queue.Count
        member this.GetEnumerator() = queue.GetEnumerator()
        member this.GetEnumerator() =
            queue.GetEnumerator() :> System.Collections.IEnumerator


open System
open System.Threading


[<AbstractClass>]
type MsgQueueAsync<'Msg>() =
    inherit MsgQueue<'Msg>()

    let isRunning = new LockObject<_>(false)

    let mutable _sleepTime = new LockObject<_>(5u, true)

    let onErrorEvent = new Event<exn>()

    member __.OnError with get() = onErrorEvent.Publish

    member this.SleepTime
        with get() =  fst _sleepTime.Value
        and set(value) =
            _sleepTime.Value <- value, value <> 0u


    member __.IsRunning
        with get() : bool =
            isRunning.Value
        and inline private set(value) =
            isRunning.Value <- value


    abstract OnPopMsg : 'Msg -> unit

    member this.StartAsync() =
        let running = this.IsRunning
        if running then
            raise <| InvalidOperationException()
        
        this.IsRunning <- true

        async {
            try
                while this.IsRunning do
                    (this :> IDequeue<_>).TryDequeue() |> function
                    | Some msg ->
                        this.OnPopMsg(msg)
                    | None ->
                        let sleepTime, doSleep = _sleepTime.Value
                        if doSleep then
                            Thread.Sleep(int sleepTime)
            with e ->
                onErrorEvent.Trigger(e)
        } |> Async.Start

    member this.Stop() =
        this.IsRunning <- false