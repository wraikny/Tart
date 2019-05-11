namespace wraikny.Tart.Helper.Math

[<Struct>]
type VectorClass< ^a, ^Vec, ^Ma, ^MVec
    when ^a   : (static member (~-) : ^a -> ^a)
    and  ^a   : (static member (+) : ^a * ^a -> ^a)
    and  ^a   : (static member (-) : ^a * ^a -> ^a)
    and  ^a   : (static member (*) : ^a * ^a -> ^a)
    and  ^a   : (static member (/) : ^a * ^a -> ^a)
    and  ^Vec : (static member (~-) : ^Vec -> ^Vec)
    and  ^Vec : (static member (+) : ^Vec * ^Vec -> ^Vec)
    and  ^Vec : (static member (-) : ^Vec * ^Vec -> ^Vec)
    and  ^Vec : (static member (*) : ^Vec * ^Vec -> ^Vec)
    and  ^Vec : (static member (/) : ^Vec * ^Vec -> ^Vec)
    > = {
    Zero : unit -> ^Vec
    FromScalar : ^a -> ^Vec
    Dot : ^Vec -> ^Vec -> ^a
    Axes : unit -> (^Vec -> ^a) list
    Map : (^a -> ^Ma) -> ^Vec -> ^MVec
}


type VectorBuiltin = VectorBuiltin with
    static member inline VectorImpl(_ : ^a Vec2) : VectorClass< ^a, ^a Vec2, ^Ma, ^Ma Vec2 > =
        {
            Zero = Vec2.zero
            FromScalar = Vec2.fromScalar
            Dot = Vec2.dot
            Axes = Vec2.axes
            Map = Vec2.map
        }
    
    static member inline VectorImpl(_ : ^a Vec3): VectorClass< ^a, ^a Vec3, ^Ma, ^Ma Vec3 > =
        {
            Zero = Vec3.zero
            FromScalar = Vec3.fromScalar
            Dot = Vec3.dot
            Axes = Vec3.axes
            Map = Vec3.map
        }

    static member inline VectorImpl(_ : ^a Vec4): VectorClass< ^a, ^a Vec4, ^Ma, ^Ma Vec4 > =
        {
            Zero = Vec4.zero
            FromScalar = Vec4.fromScalar
            Dot = Vec4.dot
            Axes = Vec4.axes
            Map = Vec4.map
        }


[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module VectorClass =
    [<CompiledName "GetImpl">]
    let inline getImpl
        (builtin : ^Builtin)
        (dummy : VectorClass< ^a, ^Vec, ^Ma, ^MVec > )
        : VectorClass< ^a, ^Vec, ^Ma, ^MVec >
        =
        (
            (^Builtin or ^Vec) : (static member VectorImpl : ^Vec -> VectorClass< ^a, ^Vec, ^Ma, ^MVec >)
                (Unchecked.defaultof< ^Vec >)
        )

    [<CompiledName "Zero">]
    let inline zero() : ^Vec =
        ( getImpl VectorBuiltin
            (Unchecked.defaultof<VectorClass< ^a, ^Vec, ^Ma, ^MVec >>)
        ).Zero()

    [<CompiledName "FromScalar">]
    let inline fromScalar(a : ^a) : ^Vec =
        ( getImpl VectorBuiltin
            (Unchecked.defaultof<VectorClass< ^a, ^Vec, ^Ma, ^MVec >>)
        ).FromScalar a

    [<CompiledName "Dot">]
    let inline dot (a : ^Vec) (b : ^Vec) : ^a =
        ( getImpl VectorBuiltin
            (Unchecked.defaultof<VectorClass< ^a, ^Vec, ^Ma, ^MVec >>)
        ).Dot a b

    [<CompiledName "Axes">]
    let inline axes() : (^Vec -> ^a) list =
        ( getImpl VectorBuiltin
            (Unchecked.defaultof<VectorClass< ^a, ^Vec, ^Ma, ^MVec >>)
        ).Axes()

    [<CompiledName "Map">]
    let inline map (f : ^a -> ^Ma) (v : ^Vec) : ^MVec =
        ( getImpl VectorBuiltin
            (Unchecked.defaultof<VectorClass< ^a, ^Vec, ^Ma, ^MVec >>)
        ).Map f v

    [<CompiledName "SquaredLength">]
    let inline squaredLength (v : ^Vec) : ^a =
        dot v v

    [<CompiledName "Length">]
    let inline length (v : ^Vec) : ^b =
        sqrt (squaredLength v)

    [<CompiledName "Normalize">]
    let inline normalize (v : ^Vec) : ^Vec =
        v / fromScalar (length v)