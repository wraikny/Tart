namespace wraikny.Tart.Core

open System

[<Interface>]
type internal IEnvCore =
    abstract Random : Random

[<Class>]
type public Environment<'ViewMsg>() =
    
    let mutable random : Random = new Random()
    let mutable updater : IMsgSender<'ViewMsg> option = None

    member __.Updater
        with get() = updater

    member this.SetUpdater(updater' : #IMsgSender<_>) =
        updater <- Some(updater' :> IMsgSender<_>)
        this

    member this.SetRandom(random' : Random) =
        random <- random'
        this

    static member Initialize<'ViewMsg>() = new Environment<'ViewMsg>()

    interface IEnvCore with
        member this.Random
            with get() = random