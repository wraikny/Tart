namespace wraikny.Tart.Core

open wraikny.Tart.Helper.Wrapper
open System.Collections.Concurrent



[<Class>]
type private CoreMessenger<'Msg>() =
    let msgQueue = new ConcurrentQueue<'Msg>()

    member __.PushMsg(msg) =
        msgQueue.Enqueue(msg)


    member __.TryPopMsg() : 'Msg option =
        let success, result = msgQueue.TryDequeue()
        if success then
            Some result
        else
            None



[<Class>]
type private Messenger<'Msg, 'ViewMsg, 'Model, 'ViewModel>(environment, coreFuncs) =

    let coreFuncs : CoreProgram<_, _, _, _> = coreFuncs

    let environment : Environment<'ViewMsg> = environment

    let coreMessenger = new CoreMessenger<'Msg>()

    let mutable lastModel :'Model option = None

    let modelQueue = new FixedSizeQueue<'Model>(1)

    let isRunning = new LockObject<bool>(false)

    do
        modelQueue.Enqueue(coreFuncs.init)


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
                coreMessenger.TryPopMsg() |> function
                | None -> model

                | Some(msg) ->
                    let newModel, cmd = coreFuncs.update msg model

                    cmd |> Cmd.execute
                        (this :> IMsgSender<_>)
                        environment

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


    interface IMsgSender<'Msg> with
        member this.PushMsg(msg) = coreMessenger.PushMsg(msg)
    

    interface IMessenger<'Msg, 'ViewModel> with
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


module Messenger =
    let createMessenger (environment) (coreFuncs) =
        (new Messenger<_, _, _, _>(environment, coreFuncs))
        :> IMessenger<_, _>


    let buildMessenger (envBuilder) (coreFuncs) =
        (new Messenger<_, _, _, _>(envBuilder |> EnvironmentBuilder.build, coreFuncs))
        :> IMessenger<_, _>