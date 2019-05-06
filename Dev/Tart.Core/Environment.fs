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


[<Struct>]
type EnvironmentBuilder<'ViewMsg> =
    {
        seed : int
        updater : IMsgSender<'ViewMsg> option
    }


[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module EnvironmentBuilder =
    [<CompiledName "Builder">]
    let build (builder) =
        let env = new Environment<_>()
        env.SetRandom(new Random(builder.seed)) |> ignore

        builder.updater |> function
        | None -> ()
        | Some updater -> env.SetUpdater(updater) |> ignore
        env