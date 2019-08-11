namespace wraikny.Tart.Core.Libraries

open wraikny.Tart.Helper
open wraikny.Tart.Core


type 'a TartTask =
    internal {
        x : IEnvironment -> Async<'a>
    }

type WithEnvBuiltin = WithEnvBuiltin with
    static member WithEnvImpl(x : 'a Random.Generator, env : IEnvironment) : 'a =
        x.F env.Random


module TartTask =
    open System.ComponentModel

    [<EditorBrowsable(EditorBrowsableState.Never)>]
    [<CompiledName "__Init">]
    /// Dont call for user
    let __init x = { x = x }


    [<CompiledName "Init">]
    let init (a : Async<'a>) = { x = fun _ -> a }


    let inline private __withEnv (_ : ^Builtin) (f : 'a -> Async<'b>) (withEnv : ^x) : 'b TartTask  =
        __init (fun env ->
            f ( (^Builtin or ^x) : (static member WithEnvImpl : ^x * IEnvironment -> 'a) (withEnv, env))
        )

    [<CompiledName "FromEnv">]
    let inline withEnv (f : 'a -> Async<'b>) (withEnv : ^x) : 'b TartTask  =
        __withEnv WithEnvBuiltin f withEnv

        
    [<CompiledName "PerformUnwrap">]
    let performUnwrap (msg : 'a -> 'Msg) (a : 'a TartTask) : Cmd<'Msg, _> =
        Cmd.initMsg (fun env pushMsg ->
            async {
                try
                    let! r = a.x(env)
                    pushMsg (msg r)
                with e ->
                    System.Console.WriteLine(e)
            }
            |> Async.Start
        )

    [<CompiledName "Perform">]
    let perform (errorMsg : exn -> 'Msg) (okMsg : 'a -> 'Msg) (a : 'a TartTask) : Cmd<'Msg, _> =
        Cmd.initMsg (fun env pushMsg ->
            async {
                try
                    let! r = a.x(env)
                    pushMsg (okMsg r)
                with e ->
                    try
                        pushMsg (errorMsg e)
                    with e' ->
                        System.Console.WriteLine(e)
                        System.Console.WriteLine(e')
            }
            |> Async.Start
        )
