namespace wraikny.Tart.Core

open System.Threading
open wraikny.Tart.Helper.Utils
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
type private Messenger<'Msg, 'ViewMsg, 'Model, 'ViewModel>
    (environment, corePrograms) =

    let corePrograms : CoreProgram<_, _, _, _> = corePrograms

    let environment : Environment<'ViewMsg> = environment

    let coreMessenger = new CoreMessenger<'Msg>()

    let mutable lastModel :'Model option = None

    let modelQueue = new FixedSizeQueue<'Model>(1)

    let isRunning = new LockObject<bool>(false)

    let mutable sleepTime = 5


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
                    let newModel, cmd = corePrograms.update msg model

                    cmd |> Cmd.execute
                        (this :> IMsgSender<_>)
                        environment

                    modelQueue.Enqueue(newModel)
                    Thread.Sleep(sleepTime)
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


    member private this.InitializeModel() =
        let model, cmd = corePrograms.init
        
        cmd |> Cmd.execute
            (this :> IMsgSender<_>)
            environment
        
        modelQueue.Enqueue(model)

        model


    interface IMsgSender<'Msg> with
        member this.PushMsg(msg) = coreMessenger.PushMsg(msg)
    

    interface IMessenger<'Msg, 'ViewModel> with
        member this.SleepTime
            with get() = sleepTime
            and  set(value) = sleepTime <- value

        member this.TryViewModel
            with get() =
                modelQueue.TryDequeue()
                |> Option.map corePrograms.view

        member this.IsRunning
            with get() = this.IsRunning

        member this.StartAsync() =
            this.InitializeModel()
            |> this.MainLoop

        member this.ResumeAsync() =
            lastModel |> function
            | Some model -> this.MainLoop(model)
            | None ->
                (this :> IMessenger<'Msg, 'ViewModel>).StartAsync()

        member this.Stop() =
            this.IsRunning <- false


[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Messenger =
    [<CompiledName "CreateMessenger">]
    let createMessenger (environment) (corePrograms) =
        (new Messenger<_, _, _, _>(environment, corePrograms))
        :> IMessenger<_, _>


    [<CompiledName "BuildMessenger">]
    let buildMessenger (envBuilder) (corePrograms) =
        (new Messenger<_, _, _, _>(envBuilder |> EnvironmentBuilder.build, corePrograms))
        :> IMessenger<_, _>