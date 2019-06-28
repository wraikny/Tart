namespace wraikny.Tart.Core.Libraries

open wraikny.Tart.Helper.Monad


[<Struct>]
type Never = Never


type Task<'Ok, 'Error> =
    {
        f : unit -> Result<'Ok, 'Error>
    }
    

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Task =
    [<CompiledName "Init">]
    let inline init (f) = { f = f }

    let inline internal f (task) = task.f


    // let internal isAsync (task) = task.isAsync

    [<CompiledName "Succeed">]
    let inline succeed (a : 'a) : Task<'a, 'Error> =
        (fun _ -> Result.Ok a) |> init


    [<CompiledName "Fail">]
    let inline fail (x : 'x) : Task<'Ok, 'x> =
        (fun _ -> Result.Error x) |> init


    [<CompiledName "Bind">]
    let inline bind (f : 'a -> Task<'b, 'x>) (task : Task<'a, 'x>) : Task<'b, 'x> =
        (fun _ ->
            task.f() |> function
            | Ok v -> (f v).f()
            | Error e -> Result.Error e
        )
        |> init

    type TaskBuilder() =
        member __.Bind(x, k) = bind x k
        member __.Return(x) = succeed x
        member __.ReturnFrom(x) = x
        member __.Delay(f) = f()
        member __.Combine(a, b) =
            a.f() |> function
            | Ok _ -> a
            | Error _ -> b()

        member __.For(inp, f) =
            seq {for a in inp -> f a}
        member __.Yield(x) = succeed x
        member __.YieldFrom(x) = x

    let task = new TaskBuilder()

    [<CompiledName "Map">]
    let inline map (f : 'a -> 'b) (task : Task<'a, 'Error>) : Task<'b, 'Error> =
        task.f >> Result.map f
        |> init


    [<CompiledName "Map2">]
    let inline map2 (f : 'a -> 'b -> 'c)
        (task1 : Task<'a, 'Error>)
        (task2 : Task<'b, 'Error>)
        : Task<'c, 'Error> =
        fun _ -> either {
            let! ok1 = task1.f()
            let! ok2 = task2.f()
            return (f ok1 ok2)
        }
        |> init


    [<CompiledName "Map3">]
    let inline map3 (f : 'a -> 'b -> 'c -> 'd)
        (task1 : Task<'a, 'Error>)
        (task2 : Task<'b, 'Error>)
        (task3 : Task<'c, 'Error>)
        : Task<'d, 'Error> =
        fun _ -> either {
            let! ok1 = task1.f()
            let! ok2 = task2.f()
            let! ok3 = task3.f()
            return (f ok1 ok2 ok3)
        }
        |> init


    [<CompiledName "Map4">]
    let inline map4 (f : 'a -> 'b -> 'c -> 'd -> 'e)
        (task1 : Task<'a, 'Error>)
        (task2 : Task<'b, 'Error>)
        (task3 : Task<'c, 'Error>)
        (task4 : Task<'d, 'Error>)
        : Task<'e, 'Error> =
        fun _ -> either {
            let! ok1 = task1.f()
            let! ok2 = task2.f()
            let! ok3 = task3.f()
            let! ok4 = task4.f()
            return (f ok1 ok2 ok3 ok4)
        }
        |> init


    [<CompiledName "Map5">]
    let inline map5 (f : 'a -> 'b -> 'c -> 'd -> 'e -> 'f)
        (task1 : Task<'a, 'Error>)
        (task2 : Task<'b, 'Error>)
        (task3 : Task<'c, 'Error>)
        (task4 : Task<'d, 'Error>)
        (task5 : Task<'e, 'Error>)
        : Task<'f, 'Error> =
        fun _ -> either {
            let! ok1 = task1.f()
            let! ok2 = task2.f()
            let! ok3 = task3.f()
            let! ok4 = task4.f()
            let! ok5 = task5.f()
            return (f ok1 ok2 ok3 ok4 ok5)
        }
        |> init

    open System.Collections.Generic

    [<CompiledName "Sequence">]
    let inline sequence (tasks : Task<'a, 'x> list) : Task<'a list, 'x> =
        let rec doTasks result tasks =
            tasks |> function
            | [] -> Result.Ok result
            | task::xs ->
                task.f() |> function
                | Ok v -> doTasks (v::result) xs
                | Error e -> Result.Error e

        (fun _ -> doTasks [] tasks)
        |> init


    [<CompiledName "OnError">]
    let inline onError (f : 'x -> Task<'a, 'y>) (task : Task<'a, 'x>) : Task<'a, 'y> =
        (fun _ ->
            task.f() |> function
            | Ok v -> Result.Ok v
            | Error e -> (f e).f()
        )
        |> init

            
    [<CompiledName "MapError">]
    let inline mapError (f : 'x -> 'y) (task : Task<'a, 'x>) : Task<'a, 'y> =
        task.f >> Result.mapError f
        |> init


    open wraikny.Tart.Core
        
        
    [<CompiledName "Perform">]
    let perform
        (f : 'a -> 'Msg)
        (task : Task<'a, Never>)
        : Cmd<'Msg, _> =
        
        Cmd.init [
            (fun _ pushMsg ->
                async {
                    task.f() |> function
                    | Ok v ->
                        f v |> pushMsg
                    | _ -> ()
                }
                |> Async.Start
            )
        ] []


    [<CompiledName "Attempt">]
    let attempt
        (f : Result<'Ok, 'Error> -> 'Msg)
        (task : Task<'Ok, 'Error>)
        : Cmd<'Msg, _> =

        Cmd.init [
            (fun _ pushMsg ->
                async {
                    task.f()
                    |> f
                    |> pushMsg
                }
                |> Async.Start
            )
        ] []