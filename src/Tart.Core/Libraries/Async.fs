namespace wraikny.Tart.Core.Libraries

open wraikny.Tart.Core

module Async =
    let toMaybe (a : Async<'a>) : Async<'a option> =
        async {
            try
                let! r = a
                return Some r
            with _ ->
                return None
        }

    let toResult (a : Async<'a>) : Async<Result<'a, exn>> =
        async {
            try
                let! r = a
                return Ok r
            with e ->
                return Error e
        }
        
        
    [<CompiledName "Perform">]
    let perform (msg : 'a -> 'Msg) (a : Async<'a>) : Cmd<'Msg, _> =
        Cmd.initMsg (fun _ pushMsg ->
            async {
                try
                    let! r = a
                    pushMsg (msg r)
                with e ->
                    System.Console.WriteLine(e)
            }
            |> Async.Start
        )