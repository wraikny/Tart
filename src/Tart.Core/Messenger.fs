namespace wraikny.Tart.Core

open System
open System.Threading
open wraikny.Tart.Helper.Collections
open wraikny.Tart.Helper.Utils

open FSharpPlus


type Messenger<'Msg, 'ViewMsg, 'Model, 'ViewModel>
    private (env : ITartEnv, corePrograms : Program<_, _, _, _>) =
    let msgEvent = Event<'Msg>()

    let mutable lastModelExist = false
    let mutable lastModel = Unchecked.defaultof<'Model>

    let modelQueue = FixedSizeQueue<'Model>(1)
    let viewMsgQueue = MsgQueue<'ViewMsg>()
    
    let msgQueue = new MsgQueueAsync<'Msg>()

    do
        msgQueue.OnPopMsg.Add(fun msg ->
            let newModel, cmd = corePrograms.update msg lastModel
                        
            cmd |> Cmd.execute msgQueue.Enqueue viewMsgQueue.Enqueue { env = env; cts = msgQueue.CancellationTokenSource}
                        
            modelQueue.Enqueue(newModel)
                
            lastModel <- newModel

            msgEvent.Trigger(msg)
        )

        //msgQueue.OnError.Add(fun _ -> env.SetCTS(null))

    let viewModelNotifier =
        Notifier<'ViewModel>(fun () ->
            modelQueue.TryDequeue()
            |>> corePrograms.view
        )

    let viewMsgNotifier = Notifier<'ViewMsg>(viewMsgQueue.TryDequeue)

    static member Create(env, program) =
        new Messenger<_, _, _, _>(env, program)
        :> IMessenger<_, _, _>

    static member Create(env, program) =
        new Messenger<_, _, _, _>(env |> TartEnv.build, program)
        :> IMessenger<_, _, _>


    member private this.InitModel() =
        let model, cmd = corePrograms.init
        
        cmd |>
            Cmd.execute
                msgQueue.Enqueue
                viewMsgQueue.Enqueue
                { env = env; cts = msgQueue.CancellationTokenSource}
        
        modelQueue.Enqueue(model)

        lastModel <- model
        lastModelExist <- true
    

    interface IMessenger<'Msg, 'ViewMsg, 'ViewModel> with
        member __.NotifyView() =
            viewMsgNotifier.PullAll()
            viewModelNotifier.Pull()

        member __.ViewModel with get() = viewModelNotifier :> IObservable<_>

        member __.Enqueue(msg) = msgQueue.Enqueue(msg)

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
            if lastModelExist then
                (msgQueue :> IQueue<_>).Clear()

            msgQueue.StartAsync()
            this.InitModel()
            //env.SetCTS(msgQueue.CancellationTokenSource)

        member this.ResumeAsync() =
            if lastModelExist then
                msgQueue.StartAsync()
                true
            else
                (this :> IMessenger<_, _>).StartAsync()
                false

        member __.Stop() =
            msgQueue.Stop()
            //env.SetCTS(null)

        member __.OnUpdated with get() = msgQueue.OnUpdated

        member __.OnError with get() = msgQueue.OnError


type MessengerBuilder = MessengerBuilder with
    static member inline Create(env : ITartEnv, program) =
        Messenger.Create(env, program)

    static member inline Create(env : TartEnvBuilder, program) =
        Messenger.Create(env, program)