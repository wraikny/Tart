namespace wraikny.Tart.Core
open wraikny.Tart.Core


/// Wrapper class for lock struct
[<Class>]
type private LockObject<'a>(value : 'a) =
    let value = value
    member val Value = value with get, set


type CoreFunctions<'Model, 'Msg, 'ViewModel> =
    {
        init : 'Model
        update : 'Msg -> 'Model -> ('Model * 'Msg Cmd)
        view : 'Model -> 'ViewModel
    }


open System.Collections.Concurrent


/// Telling msg and viewModel, between modelLoop(async) and view(mainThread).
[<Class>]
type public Messenger<'Model, 'Msg, 'ViewModel when 'Model : struct>(coreFuncs) =

    let coreFuncs : CoreFunctions<'Model, 'Msg, 'ViewModel> = coreFuncs

    let msgQueue = new ConcurrentQueue<'Msg option>()

    let mutable model = new LockObject<'Model>( coreFuncs.init )

    let mutable isRunning = new LockObject<bool>(false)

    /// Add Msg to ConcurrentQueue
    member public __.PushMsg(msg : 'Msg) : unit =
        msgQueue.Enqueue(Some msg)

    /// Get Msg from ConcurrentQueue
    member private __.PopMsg() : 'Msg option =
        let mutable result = None
        if msgQueue.TryDequeue(&result) then
            result
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
    member __.ViewModel
        with public get() =
            lock model (fun _ ->
                model.Value
                |> coreFuncs.view
            )

    /// Thread safe property of isRunning flag
    member __.IsRunning
        with public get() : bool =
            lock isRunning (fun _ -> isRunning.Value)
        and private set(value) =
            lock isRunning (fun _ -> isRunning.Value <- value)

    /// Stop asynchronous loop of model
    member public this.Stop() =
        this.IsRunning <- false

    /// Async.Start main loop
    member public this.RunAsync() =
        let running = this.IsRunning
        if running then ()
        else
            this.IsRunning <- true

            let update () =
                this.PopMsg() |> function
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