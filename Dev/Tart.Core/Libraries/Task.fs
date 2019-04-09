namespace wraikny.Tart.Core

open wraikny.Tart.Helper.Basic


type Task<'Ok, 'Error> =
    {
        f : unit -> Result<'Ok, 'Error>
    }
    

module Task =
    let internal init (f) =
        { f = f }


    let internal f (task) = task.f


    // let internal isAsync (task) = task.isAsync


    let succeed (a : 'a) : Task<'a, 'Error> =
        (fun _ -> Result.Ok a) |> init


    let fail (x : 'x) : Task<'Ok, 'x> =
        (fun _ -> Result.Error x) |> init


    let map (f : 'a -> 'b) (task : Task<'a, 'Error>) : Task<'b, 'Error> =
        task.f >> Result.map f
        |> init


    let map2 (f : 'a -> 'b -> 'c)
        (task1 : Task<'a, 'Error>)
        (task2 : Task<'b, 'Error>)
        : Task<'c, 'Error>
        =
        (fun _ ->
            task1.f() |> Result.bind(fun ok1 ->
            task2.f() |> Result.bind(fun ok2 ->
                f ok1 ok2
                |> Result.Ok
            ))
        )
        |> init
            //(
            //    [
            //        task1.isAsync
            //        task2.isAsync
            //    ]
            //    |> List.fold (||) false
            //)


    let map3 (f : 'a -> 'b -> 'c -> 'd)
        (task1 : Task<'a, 'Error>)
        (task2 : Task<'b, 'Error>)
        (task3 : Task<'c, 'Error>)
        : Task<'d, 'Error>
        =
        (fun _ ->
            task1.f() |> Result.bind(fun ok1 ->
            task2.f() |> Result.bind(fun ok2 ->
            task3.f() |> Result.bind(fun ok3 ->
                f ok1 ok2 ok3
                |> Result.Ok
            )))
        )
        |> init
            //(
            //    [
            //        task1.isAsync
            //        task2.isAsync
            //        task3.isAsync
            //    ]
            //    |> List.fold (||) false
            //)


    let map4 (f : 'a -> 'b -> 'c -> 'd -> 'e)
        (task1 : Task<'a, 'Error>)
        (task2 : Task<'b, 'Error>)
        (task3 : Task<'c, 'Error>)
        (task4 : Task<'d, 'Error>)
        : Task<'e, 'Error>
        =
        (fun _ ->
            task1.f() |> Result.bind(fun ok1 ->
            task2.f() |> Result.bind(fun ok2 ->
            task3.f() |> Result.bind(fun ok3 ->
            task4.f() |> Result.bind(fun ok4 ->
                f ok1 ok2 ok3 ok4
                |> Result.Ok
            ))))
        )
        |> init
            //(
            //    [
            //        task1.isAsync
            //        task2.isAsync
            //        task3.isAsync
            //        task4.isAsync
            //    ]
            //    |> List.fold (||) false
            //)


    let map5 (f : 'a -> 'b -> 'c -> 'd -> 'e -> 'f)
        (task1 : Task<'a, 'Error>)
        (task2 : Task<'b, 'Error>)
        (task3 : Task<'c, 'Error>)
        (task4 : Task<'d, 'Error>)
        (task5 : Task<'e, 'Error>)
        : Task<'f, 'Error>
        =
        (fun _ ->
            task1.f() |> Result.bind(fun ok1 ->
            task2.f() |> Result.bind(fun ok2 ->
            task3.f() |> Result.bind(fun ok3 ->
            task4.f() |> Result.bind(fun ok4 ->
            task5.f() |> Result.bind(fun ok5 ->
                f ok1 ok2 ok3 ok4 ok5
                |> Result.Ok
            )))))
        )
        |> init
            //(
            //    [
            //        task1.isAsync
            //        task2.isAsync
            //        task3.isAsync
            //        task4.isAsync
            //        task5.isAsync
            //    ]
            //    |> List.fold (||) false
            //)


    let andThen (f : 'a -> Task<'b, 'x>) (task : Task<'a, 'x>) : Task<'b, 'x> =
        (fun _ ->
            task.f() |> function
            | Ok v -> (f v).f()
            | Error e -> Result.Error e
        )
        |> init


    let sequence (tasks : Task<'a, 'x> list) : Task<'a list, 'x> =
        let rec doTasks result tasks =
            tasks |> function
            | [] -> Result.Ok result
            | task::xs ->
                task.f() |> function
                | Ok v -> doTasks (v::result) xs
                | Error e -> Result.Error e

        (fun _ -> doTasks [] tasks)
        |> init


    let oneError (f : 'x -> Task<'a, 'y>) (task : Task<'a, 'x>) : Task<'a, 'y> =
        (fun _ ->
            task.f() |> function
            | Ok v -> Result.Ok v
            | Error e -> (f e).f()
        )
        |> init

            
    let mapError (f : 'x -> 'y) (task : Task<'a, 'x>) : Task<'a, 'y> =
        task.f >> Result.mapError f
        |> init
        
        
    let perform
        (f : 'a -> 'Msg)
        (task : Task<'a, Never>)
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