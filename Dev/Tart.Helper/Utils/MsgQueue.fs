namespace wraikny.Tart.Helper.Utils

/// Telling msg
[<Interface>]
type IMsgQueue<'T> =
    /// Add Msg
    abstract Enqueue : 'T -> unit



open System.Collections.Concurrent


type MsgQueue<'T>() =
    let queue = new ConcurrentQueue<'T>()

    member __.TryPopMsg() : 'T option =
        queue.TryDequeue() |> function
        | true, result -> Some result
        | false, _ -> None

    interface IMsgQueue<'T> with
        member __.Enqueue(msg) = queue.Enqueue(msg)



[<AbstractClass>]
type MsgQueueSync<'Msg>() =
    inherit MsgQueue<'Msg>()

    abstract OnPopMsg : 'Msg -> unit

    member public this.Update() =
        let rec update () =
            this.TryPopMsg() |> function
            | Some(msg) ->
                this.OnPopMsg(msg)
                update ()
            | None -> ()

        update ()


open System
open System.Threading


[<AbstractClass>]
type MsgQueueAsync<'Msg>() =
    inherit MsgQueue<'Msg>()

    let isRunning = new LockObject<_>(false)

    let mutable _sleepTime = new LockObject<_>(5u, true)

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

    member this.Start() =
        let running = this.IsRunning
        if running then
            raise <| new InvalidOperationException()
        else
            this.IsRunning <- true

            async {
                while this.IsRunning do
                    this.TryPopMsg() |> function
                    | Some msg ->
                        this.OnPopMsg(msg)
                    | None ->
                        let sleepTime, doSleep = _sleepTime.Value
                        if doSleep then
                            Thread.Sleep(int sleepTime)
            } |> Async.Start
        
        not running

    member this.Stop() =
        let running = this.IsRunning
        if running then
            this.IsRunning <- false
        else
            raise <| new InvalidOperationException()