namespace wraikny.Tart.Helper.Math

// https://7colou.red/blog/2018/02-14-fsharp-typeclasses/index.html

[<Struct>]
type Vector< ^a, ^Vec, ^Ma, ^MVec
    when ^a   : (static member (~-) : ^a -> ^a)
    and  ^a   : (static member (+) : ^a * ^a -> ^a)
    and  ^a   : (static member (-) : ^a * ^a -> ^a)
    and  ^a   : (static member (*) : ^a * ^a -> ^a)
    and  ^a   : (static member (/) : ^a * ^a -> ^a)
    and  ^Vec : (static member (~-) : ^Vec -> ^Vec)
    and  ^Vec : (static member (+) : ^Vec * ^Vec -> ^Vec)
    and  ^Vec : (static member (-) : ^Vec * ^Vec -> ^Vec)
    and  ^Vec : (static member (*) : ^Vec * ^Vec -> ^Vec)
    and  ^Vec : (static member ( .* ) : ^a * ^Vec -> ^Vec)
    and  ^Vec : (static member ( *. ) : ^Vec * ^a -> ^Vec)
    and  ^Vec : (static member (/) : ^Vec * ^Vec -> ^Vec)
    and  ^Vec : (static member (./) : ^a * ^Vec -> ^Vec)
    and  ^Vec : (static member (/.) : ^Vec * ^a -> ^Vec)
    > =
    {
        Init1 : ^a -> ^Vec
        Dot : ^Vec -> ^Vec -> ^a
        Axes : unit -> (^Vec -> ^a) list
    }


type VectorBuiltin = VectorBuiltin with
    static member inline VectorImpl(_ : ^a Vec2) : Vector< ^a, ^a Vec2, ^Ma, ^Ma Vec2 > =
        {
            Init1 = Vec2.init1
            Dot = Vec2.dot
            Axes = Vec2.axes
        }
    
    static member inline VectorImpl(_ : ^a Vec3): Vector< ^a, ^a Vec3, ^Ma, ^Ma Vec3 > =
        {
            Init1 = Vec3.init1
            Dot = Vec3.dot
            Axes = Vec3.axes
        }

    static member inline VectorImpl(_ : ^a Vec4): Vector< ^a, ^a Vec4, ^Ma, ^Ma Vec4 > =
        {
            Init1 = Vec4.init1
            Dot = Vec4.dot
            Axes = Vec4.axes
        }


open FSharpPlus


[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Vector =
    [<CompiledName "GetImpl">]
    let inline getImpl
        (_builtin : ^Builtin)
        (_dummy : Vector< ^a, ^Vec, ^Ma, ^MVec > )
        : Vector< ^a, ^Vec, ^Ma, ^MVec >
        =
        (
            (^Builtin or ^Vec) : (static member VectorImpl : ^Vec -> Vector< ^a, ^Vec, ^Ma, ^MVec >)
                (Unchecked.defaultof< ^Vec >)
        )


    let inline private init1(a : ^a) : ^Vec =
        ( getImpl VectorBuiltin
            (Unchecked.defaultof<Vector< ^a, ^Vec, _, _ >>)
        ).Init1 a


    [<CompiledName "Zero">]
    let inline zero() : ^Vec = init1(zero)

    [<CompiledName "One">]
    let inline one() : ^Vec = init1(one)

    [<CompiledName "Dot">]
    let inline dot (a : ^Vec) (b : ^Vec) : ^a =
        ( getImpl VectorBuiltin
            (Unchecked.defaultof<Vector< ^a, ^Vec, _, _ >>)
        ).Dot a b

    [<CompiledName "Axes">]
    let inline axes() : (^Vec -> ^a) list =
        ( getImpl VectorBuiltin
            (Unchecked.defaultof<Vector< ^a, ^Vec, _, _ >>)
        ).Axes()

    [<CompiledName "SquaredLength">]
    let inline squaredLength (v : ^Vec) : ^a =
        dot v v

    [<CompiledName "Length">]
    let inline length (v : ^Vec) : ^b =
        sqrt <| squaredLength v

    [<CompiledName "Normalize">]
    let inline normalize (v : ^Vec) : ^Vec =
        v /. length v