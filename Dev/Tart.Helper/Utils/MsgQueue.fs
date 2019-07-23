﻿namespace wraikny.Tart.Helper.Utils

/// Telling msg
type IMsgQueue<'T> = interface
    /// Add Msg
    abstract Enqueue : 'T -> unit
end

open System.Collections.Concurrent


type MsgQueue<'T>() =
    let queue = new ConcurrentQueue<'T>()

    member __.TryDequeue() : 'T option =
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
            this.TryDequeue() |> function
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

    member this.StartAsync() =
        let running = this.IsRunning
        if running then
            raise <| InvalidOperationException()
        
        this.IsRunning <- true

        async {
            try
                while this.IsRunning do
                    this.TryDequeue() |> function
                    | Some msg ->
                        this.OnPopMsg(msg)
                    | None ->
                        let sleepTime, doSleep = _sleepTime.Value
                        if doSleep then
                            Thread.Sleep(int sleepTime)
            with e ->
                System.Console.WriteLine(e)
                raise e
        } |> Async.Start

    member this.Stop() =
        this.IsRunning <- false