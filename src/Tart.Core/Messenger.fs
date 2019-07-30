namespace wraikny.Tart.Core

open System
open System.Reactive
open System.Threading
open wraikny.Tart.Helper.Collections
open wraikny.Tart.Helper.Utils

open FSharpPlus


[<Class>]
type private Messenger<'Msg, 'ViewMsg, 'Model, 'ViewModel>
    (environment : IEnvironment, corePrograms : CoreProgram<_, _, _, _>) =
    // inherit MsgQueueAsync<'Msg>()

    let subject = new Subjects.Subject<'Msg>()

    let mutable lastModelExist = false
    let mutable lastModel = Unchecked.defaultof<'Model>

    let modelQueue = new FixedSizeQueue<'Model>(1)
    let viewMsgQueue = new MsgQueue<'ViewMsg>()

    let msgQueue =
        { new MsgQueueAsync<'Msg>() with
            override x.OnPopMsg(msg) =
                subject.OnNext(msg)
                
                let newModel, cmd = corePrograms.update msg lastModel
                        
                cmd |> Cmd.execute x viewMsgQueue environment
                        
                (modelQueue :> IEnqueue<_>).Enqueue(newModel)
                
                lastModel <- newModel
        }

    let viewModelNotifier =
        new Notifier<'ViewModel>(
            { new IDequeue<'ViewModel> with
                member __.TryDequeue() =
                    (modelQueue :> IDequeue<_>).TryDequeue()
                    |>> corePrograms.view
            })

    let viewMsgNotifier = new Notifier<'ViewMsg>(viewMsgQueue)


    member this.InitModel() =
        let model, cmd = corePrograms.init
        
        cmd |> Cmd.execute msgQueue viewMsgQueue environment
        
        (modelQueue :> IEnqueue<_>).Enqueue(model)

        lastModel <- model
        lastModelExist <- true
    

    interface IMessenger<'Msg, 'ViewMsg, 'ViewModel> with
        member __.Enqueue(msg) = (msgQueue :> IEnqueue<_>).Enqueue(msg)

        member __.Msg with get() = subject :> IObservable<_>
        member __.ViewModel with get() = viewModelNotifier :> IObservable<_>
        member __.ViewMsg with get() = viewMsgNotifier :> IObservable<_>

        member __.NotifyView() =
            viewMsgNotifier.PullAll()
            viewModelNotifier.Pull()

        member __.Dispose() =
            msgQueue.Stop()
            subject.Dispose()

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
                (this :> IMessenger<_, _, _>).StartAsync()
                false

        member __.Stop() = msgQueue.Stop()


[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Messenger =
    [<CompiledName "CreateMessenger">]
    let createMessenger (environment : wraikny.Tart.Core.Environment) (corePrograms) =
        (new Messenger<_, _, _, _>(environment :> IEnvironment, corePrograms))
        :> IMessenger<_, _, _>


    [<CompiledName "BuildMessenger">]
    let buildMessenger (envBuilder) (corePrograms) =
        (new Messenger<_, _, _, _>(envBuilder |> EnvironmentBuilder.build, corePrograms))
        :> IMessenger<_, _, _>
