namespace wraikny.Tart.Helper.Utils

type LockObject<'T>(value : 'T) =
    let mutable value = value
    let _lock = System.Object()

    member public __.Value
        with get() =
            lock _lock <| fun _ ->
                value

        and set(value_) =
            lock _lock <| fun _ ->
                value <- value_