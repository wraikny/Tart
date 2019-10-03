namespace wraikny.Tart.Helper

open System.Collections.Generic

type ObjectsPool<'a> private (stack, create) =
    let pool: Stack<'a> = stack

    new(create) = new ObjectsPool<'a>(Stack<_>(), create)

    new(count: int, create) =
        let pool = Stack<_>()
        for i = 1 to count do
            pool.Push(create())
        new ObjectsPool<'a>(pool, create)

    member __.Pop() =
        if pool.Count = 0 then
            create()
        else
            pool.Pop()

    member __.Push(a) =
        pool.Push(a)

    member __.Clear() = pool.Clear()
