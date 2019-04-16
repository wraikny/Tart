namespace wraikny.Tart.Core

open System

[<Interface>]
type internal IEnvCore =
    abstract Random : Random

[<Class>]
type Environment<'ViewMsg>() =

    member val internal Updater : IMsgSender<'ViewMsg> option = None with get, set

    static member Initialize<'ViewMsg>() = new Environment<'ViewMsg>()

    interface IEnvCore with
        member val Random = new Random()