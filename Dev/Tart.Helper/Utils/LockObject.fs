namespace wraikny.Tart.Helper.Utils

[<Class>]
type LockObject<'T>(value : 'T) =
    let mutable value = value
    let _lock = new System.Object()

    member public __.Value
        with get() =
            lock _lock <| fun _ ->
                value

        and set(value_) =
            lock _lock <| fun _ ->
                value <- value_


[<AbstractClass; Sealed>]
type StaticLock private() =
    static member private _lockObj = new System.Object()

    static member Lock f =
        lock StaticLock._lockObj <|
            fun _ -> f()

    static member inline Printfn s =
        StaticLock.Lock <| fun _ ->
            printfn "%s" s



type AsyncDebugPrinter(isActive, label) =
    let label = label
    let mutable isActive = isActive

    member __.IsActive
        with get() = isActive
        and  set(value) = isActive <- value

    member this.Print(s) =
        if isActive then
            StaticLock.Printfn(sprintf "[%s] %s" label s)

