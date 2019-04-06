namespace wraikny.Tart.Core
open wraikny.Tart.Core


/// Wrapper class for lock struct
[<Class>]
type private LockObject<'a>(value : 'a) =
    let value = value
    member val Value = value with get, set


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
    abstract ViewModel : 'ViewModel
    /// Thread safe getter of isRunning flag
    abstract IsRunning : bool with get
    /// Async.Start main loop
    abstract RunAsync : unit -> bool
    /// Stop asynchronous main loop
    abstract Stop : unit -> unit


open System.Collections.Concurrent


[<Class>]
type private Messenger<'Model, 'Msg, 'ViewModel when 'Model : struct>(coreFuncs) =

    let coreFuncs : CoreFunctions<'Model, 'Msg, 'ViewModel> = coreFuncs

    let msgQueue = new ConcurrentQueue<'Msg>()

    let mutable model = new LockObject<'Model>( coreFuncs.init )

    let mutable isRunning = new LockObject<bool>(false)

    /// Add Msg to ConcurrentQueue
    member private __.PushMsg(msg : 'Msg) : unit =
        msgQueue.Enqueue(msg)

    /// Get Msg from ConcurrentQueue
    member private __.TryPopMsg() : 'Msg option =
        let success, result = msgQueue.TryDequeue()
        if success then
            Some result
        else
            None

    /// Thread safe property of model
    member private __.Model
        with get() =
            lock model (fun _ -> model.Value)

        and set(value) =
            lock model (fun _ ->
                model.Value <- value
            )
    
    /// Thread safe getter of ViewModel
    member private __.ViewModel
        with get() =
            lock model (fun _ ->
                model.Value
                |> coreFuncs.view
            )

    /// Thread safe property of isRunning flag
    member private __.IsRunning
        with get() : bool =
            lock isRunning (fun _ -> isRunning.Value)
        and set(value) =
            lock isRunning (fun _ -> isRunning.Value <- value)

    /// Stop asynchronous main loop
    member private this.Stop() =
        this.IsRunning <- false

    /// Async.Start main loop
    member private this.RunAsync() =
        let running = this.IsRunning
        if not running then
            this.IsRunning <- true

            let update () =
                this.TryPopMsg() |> function
                | None -> ()

                | Some(msg) ->
                    let newModel, cmd = coreFuncs.update msg this.Model

                    cmd |> Cmd.execute(fun msg -> this.PushMsg msg)

                    this.Model <- newModel

                    ()
            
            /// Main Loop
            let rec loop () =
                // exit
                if not this.IsRunning then ()
                // next
                elif msgQueue.IsEmpty then loop ()
                // update
                else
                    update ()
                    loop ()

            async {
                loop ()
            } |> Async.Start

        this.IsRunning


    interface IMessenger<'Msg, 'ViewModel> with
        member this.PushMsg(msg) = this.PushMsg(msg)

        member this.ViewModel
            with get() = this.ViewModel

        member this.IsRunning
            with get() = this.IsRunning

        member this.RunAsync() = this.RunAsync()

        member this.Stop() = this.Stop()


module IMessenger =
    let createMessenger(coreFuncs) =
        (new Messenger<_, _, _>(coreFuncs))
        :> IMessenger<_, _>