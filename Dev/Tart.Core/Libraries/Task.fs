namespace wraikny.Tart.Core


type Task<'Ok, 'Error> =
    {
        isAsync : bool
        f : unit -> Result<'Ok, 'Error>
    }
    

module Task =
    let internal init (isAsync) (f) =
        { isAsync = isAsync; f = f }

    let internal isAsync (task) = task.isAsync

    let succeed (a : 'a) : Task<'a, 'Error> =
        (fun _ -> Result.Ok a) |> (init false)

    let map (f : 'a -> 'b) (task : Task<'a, 'Error>) : Task<'b, 'Error> =
        task.f >> Result.map f
        |> init task.isAsync

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
        |> init(
            [
                task1.isAsync
                task2.isAsync
            ]
            |> List.forall (fun x -> x)
        )


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
        |> init(
            [
                task1.isAsync
                task2.isAsync
                task3.isAsync
            ]
            |> List.forall (fun x -> x)
        )


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
        |> init(
            [
                task1.isAsync
                task2.isAsync
                task3.isAsync
                task4.isAsync
            ]
            |> List.forall (fun x -> x)
        )


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
        |> init(
            [
                task1.isAsync
                task2.isAsync
                task3.isAsync
                task4.isAsync
                task5.isAsync
            ]
            |> List.forall (fun x -> x)
        )

            
        
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