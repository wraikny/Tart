namespace wraikny.Tart.Core.Libraries

open wraikny.Tart.Core

module Async =
    [<CompiledName "Perform">]
    let perform (msg : 'a -> 'Msg) (a : Async<'a>) : Cmd<'Msg, _> =
        Cmd.singleCommand (fun _ pushMsg ->
            async {
                try
                    let! r = a
                    pushMsg (msg r)
                with e ->
                    System.Console.WriteLine(e)
            }
            |> Async.Start
        )