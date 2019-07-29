namespace wraikny.Tart.Helper.Utils

[<Class>]
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


//[<AbstractClass; Sealed>]
//type StaticLock private() =
//    static member private _lockObj = System.Object()

//    static member Lock f =
//        lock StaticLock._lockObj <|
//            fun _ -> f()


type IUpdatee<'a> = interface
    abstract Update : 'a -> unit
end