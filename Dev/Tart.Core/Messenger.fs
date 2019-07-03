namespace wraikny.Tart.Core

open System.Threading
open wraikny.Tart.Helper.Collections
open wraikny.Tart.Helper.Utils



[<Class>]
type private Messenger<'Msg, 'ViewMsg, 'Model, 'ViewModel>
    (environment, corePrograms) =
    inherit MsgQueueAsync<'Msg>()

    let corePrograms : CoreProgram<_, _, _, _> = corePrograms

    let environment : IEnvironment = environment

    let mutable lastModel = Unchecked.defaultof<'Model>
    let mutable lastModelExist = false

    let modelQueue = new FixedSizeQueue<'Model>(1)

    let mutable port : IMsgQueue<'ViewMsg> option = None

    let observable = new Observable<'Msg>()

    override this.OnPopMsg(msg) =
        let newModel, cmd = corePrograms.update msg lastModel
        
        cmd |> Cmd.execute this port environment
        
        modelQueue.Enqueue(newModel)

        lastModel <- newModel

        observable.Notify(msg)


    member this.InitModel() =
        let model, cmd = corePrograms.init
        
        cmd |> Cmd.execute this port environment
        
        modelQueue.Enqueue(model)

        lastModel <- model
        lastModelExist <- true


    interface IObservable<'Msg> with
        member this.Add(o) = observable.Add(o)
        member this.Clear() = observable.Clear()
    

    interface IMessenger<'Msg, 'ViewMsg, 'ViewModel> with
        member this.SleepTime
            with get() = this.SleepTime
            and  set(value) = this.SleepTime <- value

        member __.SetPort(port_) =
            port <- Some (port_ :> IMsgQueue<'ViewMsg>)

        member this.TryPopViewModel
            with get() =
                modelQueue.TryDequeue()
                |> Option.map corePrograms.view

        member this.IsRunning
            with get() = this.IsRunning

        member this.StartAsync() =
            this.InitModel()
            this.StartAsync()

        member this.ResumeAsync() =
            if lastModelExist then
                this.StartAsync()
                true
            else
                (this :> IMessenger<_, _, _>).StartAsync()
                false

        member this.Stop() =
            this.Stop()


[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Messenger =
    [<CompiledName "CreateMessenger">]
    let createMessenger (environment : Environment) (corePrograms) =
        (new Messenger<_, _, _, _>(environment :> IEnvironment, corePrograms))
        :> IMessenger<_, _, _>


    [<CompiledName "BuildMessenger">]
    let buildMessenger (envBuilder) (corePrograms) =
        (new Messenger<_, _, _, _>(envBuilder |> EnvironmentBuilder.build, corePrograms))
        :> IMessenger<_, _, _>