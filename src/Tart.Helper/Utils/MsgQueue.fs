namespace wraikny.Tart.Helper.Utils
    
open System.Collections.Generic
open System.Collections.Concurrent


type MsgQueue<'T>() =
    let queue = ConcurrentQueue<'T>()

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


type MsgQueueAsync<'Msg>() =
    inherit MsgQueue<'Msg>()

    let mutable cts = null //new CancellationTokenSource()

    //let isRunning = new LockObject<_>(false)

    let mutable _sleepTime = new LockObject<_>(5u, true)

    let onUpdatedEvent = Event<unit>()
    let onPopMsgEvent = Event<'Msg>()
    let onErrorEvent = Event<exn>()

    member __.OnUpdated with get() = onUpdatedEvent.Publish
    member __.OnPopMsg with get() = onPopMsgEvent.Publish
    member __.OnError with get() = onErrorEvent.Publish

    member this.SleepTime
        with get() =  fst _sleepTime.Value
        and set(value) =
            _sleepTime.Value <- value, value <> 0u


    member __.CancellationTokenSource = cts

    member __.IsRunning
        with get() : bool = cts <> null

    member this.StartAsync() =
        if this.IsRunning then
            raise <| InvalidOperationException("MsgQueue is running")
        
        //this.IsRunning <- true

        cts <- new CancellationTokenSource()

        let rec loop() = async {
            try
                (this :> IDequeue<_>).TryDequeue() |> function
                | Some msg ->
                    onPopMsgEvent.Trigger(msg)
                | None ->
                    let sleepTime, doSleep = _sleepTime.Value
                    if doSleep then
                        Thread.Sleep(int sleepTime)

                onUpdatedEvent.Trigger()

                return! loop()
            with e ->
                onErrorEvent.Trigger(e)
        }
        
        Async.Start(loop(), cts.Token)

    member this.Stop() =
        if not this.IsRunning then
            raise <| InvalidOperationException("MsgQueue is not running")

        cts.Cancel()
        cts <- null

    interface System.IDisposable with
        member this.Dispose() =
            if this.IsRunning then
                this.Stop()