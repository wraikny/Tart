namespace wraikny.Tart.Core

type Task<'Ok, 'Error> =
    {
        f : unit -> Result<'Ok, 'Error>
    }
    


module Task =
    let internal init (f) =
        { f = f }

    let map (f : 'a -> 'b) (task : Task<'a, 'Error>) : Task<'b, 'Error> =
        task.f >> Result.map f
        |> init
            
        
    let perform
        (f : 'a -> 'Msg)
        (task : Task<'a, unit>)
        : 'Msg Cmd =
        
        [
            (fun pushMsg ->
                async {
                    task.f() |> function
                    | Ok v ->
                        f v |> pushMsg
                    | _ -> ()
                }
                |> Async.Start
            )
        ]
        |> Cmd.init


    let attempt
        (f : Result<'Ok, 'Error> -> 'Msg)
        (task : Task<'Ok, 'Error>)
        : 'Msg Cmd =

        [
            (fun pushMsg ->
                async {
                    task.f()
                    |> f
                    |> pushMsg
                }
                |> Async.Start
            )
        ]
        |> Cmd.init