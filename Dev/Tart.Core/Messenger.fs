namespace wraikny.Tart.Core
open wraikny.Tart.Core


type CoreFunctions<'Model, 'Msg, 'ViewModel when 'Model : struct> =
    {
        init : 'Model
        update : 'Msg -> 'Model -> ('Model * 'Msg Cmd)
        view : 'Model -> 'ViewModel
    }


/// Telling msg and viewModel, between modelLoop(async) and view(mainThread).
[<Interface>]
type public IMessenger<'Msg, 'ViewModel> =
    /// Add Msg to ConcurrentQueue
    abstract PushMsg : 'Msg -> unit
    /// Thread safe getter of ViewModel
    abstract TryViewModel : 'ViewModel option
    /// Thread safe getter of isRunning flag
    abstract IsRunning : bool with get
    /// Async.Start main loop
    abstract StartAsync : unit -> bool
    /// Async.Start main loop from last model
    abstract ResumeAsync : unit -> bool
    /// Stop asynchronous main loop
    abstract Stop : unit -> unit


open wraikny.Tart.Helper.Wrapper
open System.Collections.Concurrent


[<Class>]
type private Messenger<'Model, 'Msg, 'ViewModel when 'Model : struct>(coreFuncs) =

    let coreFuncs : CoreFunctions<'Model, 'Msg, 'ViewModel> = coreFuncs

    let mutable lastModel :'Model option = None

    let msgQueue = new ConcurrentQueue<'Msg>()

    let modelQueue = new FixedSizeQueue<'Model>(1)

    let isRunning = new LockObject<bool>(false)

    
    member private __.PushMsg(msg) =
        msgQueue.Enqueue(msg)


    member private __.TryPopMsg() : 'Msg option =
        let success, result = msgQueue.TryDequeue()
        if success then
            Some result
        else
            None


    member private __.IsRunning
        with get() : bool =
            isRunning.Value
        and set(value) =
            isRunning.Value <- value


    member private this.MainLoop initModel =
        let running = this.IsRunning
        if not running then
            this.IsRunning <- true

            let update model =
                this.TryPopMsg() |> function
                | None -> model

                | Some(msg) ->
                    let newModel, cmd = coreFuncs.update msg model

                    cmd |> Cmd.execute(fun msg -> this.PushMsg msg)

                    modelQueue.Enqueue(newModel)

                    newModel
        
            /// Main Loop
            let rec loop model =
                // stop with cache
                if not this.IsRunning then
                    lastModel <- Some model
                    ()

                // update
                else
                    model
                    |> update
                    |> loop

            async {
                initModel
                |> loop
            } |> Async.Start

        // Is started main loop in this call
        not running
        

    interface IMessenger<'Msg, 'ViewModel> with
        member this.PushMsg(msg) =
            msgQueue.Enqueue(msg)

        member this.TryViewModel
            with get() =
                modelQueue.TryDequeue()
                |> Option.map coreFuncs.view

        member this.IsRunning
            with get() = this.IsRunning

        member this.StartAsync() =
            coreFuncs.init
            |> this.MainLoop

        member this.ResumeAsync() =
            lastModel
            |> Option.defaultValue coreFuncs.init
            |> this.MainLoop

        member this.Stop() =
            this.IsRunning <- false


module IMessenger =
    let createMessenger(coreFuncs) =
        (new Messenger<_, _, _>(coreFuncs))
        :> IMessenger<_, _>