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



open System.Threading


[<AbstractClass>]
type MsgQueueAsync<'Msg>() =
    inherit MsgQueue<'Msg>()

    let isRunning = new LockObject<_>(false)

    let mutable _sleepTime = new LockObject<_>(5u)

    abstract OnPopMsg : 'Msg -> unit

    member this.MainLoop() =
        let running = this.IsRunning
        if not running then
            this.IsRunning <- true

            async {
                while this.IsRunning do
                    this.TryPopMsg() |> function
                    | Some msg ->
                        this.OnPopMsg(msg)
                    | None ->
                        ()

                    let sleepTime = this.SleepTime
                    if sleepTime <> 0u then
                        Thread.Sleep(int this.SleepTime)
            } |> Async.Start
        
        not running


    member this.SleepTime
        with get() = _sleepTime.Value
        and  set(value) = _sleepTime.Value <- value


    member __.IsRunning
        with get() : bool =
            isRunning.Value
        and set(value) =
            isRunning.Value <- value

    member this.StartAsync() =
        this.MainLoop()

    member this.Stop() =
        this.IsRunning <- false