﻿namespace wraikny.Tart.Core
open wraikny.Tart.Core.Cmd


/// Wrapper class for lock struct
[<Class>]
type private LockObject<'a>(value : 'a) =
    let value = value
    member val Value = value with get, set


type MessengerSetting<'Model, 'Msg, 'ViewModel> =
    {
        init : 'Model
        update : 'Msg -> 'Model -> ('Model * 'Msg Cmd)
        view : 'Model -> 'ViewModel
    }


open System.Collections.Concurrent


/// Telling msg and viewModel, between modelLoop(async) and view(mainThread).
[<Class>]
type public Messenger<'Model, 'Msg, 'ViewModel>(setting) =
    let setting : MessengerSetting<'Model, 'Msg, 'ViewModel> = setting

    let msgQueue = new ConcurrentQueue<'Msg option>()

    let mutable viewModel = new LockObject<'ViewModel>( setting.view(setting.init) )

    let mutable isRunning = new LockObject<bool>(true)

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
    
    /// Thread safe property of ViewModel
    member __.ViewModel
        with public get() =
            lock viewModel (fun _ ->
                viewModel.Value
            )
        and private set(value) =
            lock viewModel (fun _ ->
                viewModel.Value <- value
            )

    /// Thread safe getter of isRunning flag
    member val IsRunning : bool =
        lock isRunning (fun _ -> isRunning.Value)
        with get

    /// Exit asynchronous loop of model
    member public __.Exit() =
        lock isRunning (fun _ ->
            isRunning.Value <- false
        )

    /// StartImmediate model loop asynchronously
    member public this.RunAsync() =
        let update model =
            this.PopMsg() |> function
            | None -> model

            | Some(msg) ->
                let newModel, cmd = setting.update msg model

                cmd |> Cmd.exe(fun msg -> this.PushMsg msg)

                this.ViewModel <- setting.view newModel

                newModel
            
        /// Main Loop
        let rec loop model =
            // exit
            if not isRunning.Value then ()
            // next
            elif msgQueue.IsEmpty then loop model
            // update
            else
                model
                |> update
                |> loop

        async {
            loop setting.init
        } |> Async.StartImmediate