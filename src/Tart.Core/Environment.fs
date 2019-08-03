namespace wraikny.Tart.Core

open System

[<Interface>]
type IEnvironment =
    abstract Random : Random with get


[<Class>]
type public Environment() =
    
    let mutable random : Random = Random()

    member this.SetRandom(random' : Random) =
        random <- random'
        this

    //static member Initialize<'ViewMsg>() = Environment()

    interface IEnvironment with
        member this.Random
            with get() = random


[<Struct>]
type EnvironmentBuilder<'ViewMsg> =
    {
        seed : int
    }


[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module EnvironmentBuilder =
    [<CompiledName "Build">]
    let build (builder) =
        let env = Environment()
        env.SetRandom(Random(builder.seed)) |> ignore

        env