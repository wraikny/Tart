namespace wraikny.Tart.Core

open System
open System.Threading

[<Interface>]
type IEnvironment =
    abstract Random : Random with get


[<Class>]
type public TartEnv() =
    let mutable random : Random = Random()

    member this.SetRandom(random' : Random) =
        random <- random'

    interface IEnvironment with
        member this.Random
            with get() = random


[<Struct>]
type EnvironmentBuilder =
    {
        seed : int
    }


[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module TartEnv =
    [<CompiledName "Build">]
    let build (builder) =
        let env = TartEnv()
        env.SetRandom(Random(builder.seed)) |> ignore

        env