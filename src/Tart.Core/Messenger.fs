namespace wraikny.Tart.Core

open System
open System.Threading
open wraikny.Tart.Helper.Collections
open wraikny.Tart.Helper.Utils

open FSharpPlus



[<Class>]
type Messenger<'Msg, 'ViewMsg, 'Model, 'ViewModel>
    (environment : IEnvironment, corePrograms : CoreProgram<_, _, _, _>) =
    // inherit MsgQueueAsync<'Msg>()

    let msgEvent = Event<'Msg>()

    let mutable lastModelExist = false
    let mutable lastModel = Unchecked.defaultof<'Model>

    let modelQueue = FixedSizeQueue<'Model>(1)
    let viewMsgQueue = MsgQueue<'ViewMsg>()
    
    let msgQueue = new MsgQueueAsync<'Msg>()

    do
        msgQueue.OnPopMsg.Add(fun msg ->
            let newModel, cmd = corePrograms.update msg lastModel
                        
            cmd |> Cmd.execute msgQueue viewMsgQueue environment
                        
            (modelQueue :> IEnqueue<_>).Enqueue(newModel)
                
            lastModel <- newModel

            msgEvent.Trigger(msg)
        )

    let viewModelNotifier =
        Notifier<'ViewModel>(
            { new IDequeue<'ViewModel> with
                member __.TryDequeue() =
                    (modelQueue :> IDequeue<_>).TryDequeue()
                    |>> corePrograms.view
            })

    let viewMsgNotifier = Notifier<'ViewMsg>(viewMsgQueue)


    member private this.InitModel() =
        let model, cmd = corePrograms.init
        
        cmd |> Cmd.execute msgQueue viewMsgQueue environment
        
        (modelQueue :> IEnqueue<_>).Enqueue(model)

        lastModel <- model
        lastModelExist <- true
    

    interface IMessenger<'Msg, 'ViewMsg, 'ViewModel> with
        member __.NotifyView() =
            viewMsgNotifier.PullAll()
            viewModelNotifier.Pull()

        member __.ViewModel with get() = viewModelNotifier :> IObservable<_>

        member __.Enqueue(msg) = (msgQueue :> IEnqueue<_>).Enqueue(msg)

        member __.Msg with get() = msgEvent.Publish :> IObservable<_>
        member __.ViewMsg with get() = viewMsgNotifier :> IObservable<_>


        member __.Dispose() =
            (msgQueue :> IDisposable).Dispose()
            //subject.Dispose()

        member __.SleepTime
            with get() = msgQueue.SleepTime
            and  set(value) = msgQueue.SleepTime <- value

        member __.IsRunning
            with get() = msgQueue.IsRunning

        member this.StartAsync() =
            this.InitModel()
            msgQueue.StartAsync()

        member this.ResumeAsync() =
            if lastModelExist then
                msgQueue.StartAsync()
                true
            else
                (this :> IMessenger<_, _>).StartAsync()
                false

        member __.Stop() = msgQueue.Stop()

        member __.OnUpdated with get() = msgQueue.OnUpdated

        member __.OnError with get() = msgQueue.OnError


[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Messenger =
    [<CompiledName "Create">]
    let create (environment : wraikny.Tart.Core.IEnvironment) (corePrograms) =
        (new Messenger<_, _, _, _>(environment, corePrograms))
        :> IMessenger<_, _, _>


    [<CompiledName "Build">]
    let build (envBuilder) (corePrograms) =
        create(envBuilder |> EnvironmentBuilder.build :> IEnvironment) corePrograms
