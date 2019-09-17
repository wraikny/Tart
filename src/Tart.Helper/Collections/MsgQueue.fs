namespace wraikny.Tart.Helper.Collections

open wraikny.Tart.Helper.Utils
   
open System
open System.Threading
open System.Collections.Generic
open System.Collections.Concurrent

type IQueue<'T> =
    inherit IReadOnlyCollection<'T>

    abstract Enqueue : 'T -> unit
    abstract TryDequeue : unit -> 'T option
    abstract Clear : unit -> unit


type MsgQueue<'T>() =
    let queue = ConcurrentQueue<'T>()

    member __.TryDequeue() : 'T option =
        queue.TryDequeue() |> function
        | true, result -> Some result
        | false, _ -> None

    member __.Enqueue(msg) = queue.Enqueue(msg)

    member __.Count with get() = queue.Count
    member __.GetEnumerator() = queue.GetEnumerator()

    member __.Clear() =
        let rec loop() =
            queue.TryDequeue() |> function
            | true, _ -> loop()
            | _ -> ()

        loop()

    interface IQueue<'T> with
        member x.TryDequeue() = x.TryDequeue()

        member x.Enqueue(msg) = x.Enqueue(msg)

        member x.Count with get() = x.Count
        member x.GetEnumerator() = x.GetEnumerator()
        member x.GetEnumerator() = x.GetEnumerator() :> System.Collections.IEnumerator

        member x.Clear() = x.Clear()


type MsgQueueAsync<'Msg>() =
    inherit MsgQueue<'Msg>()

    let mutable cts = null //new CancellationTokenSource()

    //let isRunning = new LockObject<_>(false)

    let mutable _sleepTime = new LockObject<_>(5u, true)

    let onUpdatedEvent = Event<unit>()
    let onPopMsgEvent = Event<'Msg>()
    let onErrorEvent = Event<exn>()

    member __.TriggerOnError(e) = onErrorEvent.Trigger(e)

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
                this.TryDequeue() |> function
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