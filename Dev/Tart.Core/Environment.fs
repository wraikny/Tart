namespace wraikny.Tart.Core

open System

[<Interface>]
type internal IEnvironmentCore =
    abstract Random : Random


[<Class>]
type public Environment<'ViewMsg>() =
    
    let mutable random : Random = new Random()
    let mutable updater : IMsgSender<'ViewMsg> option = None

    member __.Updater
        with get() = updater

    member this.SetUpdater(updater' : IMsgSender<_>) =
        updater <- Some(updater' :> IMsgSender<_>)
        this

    member this.SetRandom(random' : Random) =
        random <- random'
        this

    static member Initialize<'ViewMsg>() = new Environment<'ViewMsg>()

    interface IEnvironmentCore with
        member this.Random
            with get() = random


type EnvironmentBuilder<'ViewMsg> =
    {
        seed : int option
        updater : IMsgSender<'ViewMsg> option
    }


module EnvironmentBuilder =
    let init() =
        {
            seed = None
            updater = None
        }

    let build (builder) =
        let env = new Environment<_>()
        builder.seed |> function
        | None -> ()
        | Some seed -> env.SetRandom(new Random(seed)) |> ignore

        builder.updater |> function
        | None -> ()
        | Some updater -> env.SetUpdater(updater) |> ignore
        env