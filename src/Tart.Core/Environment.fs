namespace wraikny.Tart.Core

open System
open System.Threading

[<Interface>]
type IEnvironment =
    abstract Random : Random with get
    abstract CTS : CancellationTokenSource with get


[<Class>]
type public TartEnv() =
    let mutable cts : CancellationTokenSource = null

    let mutable random : Random = Random()

    member this.SetRandom(random' : Random) =
        random <- random'

    member this.SetCTS(x) = cts <- x

    interface IEnvironment with
        member this.Random
            with get() = random

        member __.CTS with get() = cts


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