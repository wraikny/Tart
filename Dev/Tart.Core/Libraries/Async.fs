namespace wraikny.Tart.Core.Libraries

open wraikny.Tart.Core

module Async =
    [<CompiledName "Perform">]
    let perform (msg : 'a -> 'Msg) (a : Async<'a>) : Cmd<'Msg, _> =
        Cmd.singleCommand (fun _ pushMsg ->
            async {
                let! r = a
                pushMsg (msg r)
            }
            |> Async.Start
        )