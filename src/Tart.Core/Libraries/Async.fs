namespace wraikny.Tart.Core.Libraries

open wraikny.Tart.Helper
open wraikny.Tart.Core


type 'a TartTask =
    internal {
        x : IEnvironment -> Async<'a>
    }


module TartTask =
    open System.ComponentModel
    [<EditorBrowsable(EditorBrowsableState.Never)>]
    [<CompiledName "__Init">]
    /// Dont call for user
    let __init x = { x = x }

    [<CompiledName "Init">]
    let init (a : Async<'a>) =
        {
            x = fun _ -> a
        }

    [<CompiledName "FromEnv">]
    let inline fromEnv (f : 'a -> Async<'b>) (withEnv : ^x) : 'b TartTask  =
        __init (fun env ->
            f (^x : (static member FromEnv : ^x * IEnvironment -> 'a) (withEnv, env))
        )

        
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