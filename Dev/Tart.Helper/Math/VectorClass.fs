namespace wraikny.Tart.Helper.Math


type VectorClass< ^a, ^Vec
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
}


type VectorBuiltin = VectorBuiltin with
    static member inline VectorImpl(_ : ^a Vec2) : VectorClass< ^a, ^a Vec2 > =
        {
            Zero = Vec2.zero
            FromScalar = Vec2.fromScalar
            Dot = Vec2.dot
        }
    
    static member inline VectorImpl(_ : ^a Vec3): VectorClass< ^a, ^a Vec3 > =
        {
            Zero = Vec3.zero
            FromScalar = Vec3.fromScalar
            Dot = Vec3.dot
        }

    static member inline VectorImpl(_ : ^a Vec4): VectorClass< ^a, ^a Vec4 > =
        {
            Zero = Vec4.zero
            FromScalar = Vec4.fromScalar
            Dot = Vec4.dot
        }


[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module VectorClass =
    [<CompiledName "GetImpl">]
    let inline getImpl
        (builtin : ^Builtin)
        (dummy : VectorClass< ^a, ^Vec > )
        : VectorClass< ^a, ^Vec >
        =
        (
            (^Builtin or ^Vec) : (static member VectorImpl : ^Vec -> VectorClass< ^a, ^Vec >)
                (Unchecked.defaultof< ^Vec >)
        )

    [<CompiledName "Zero">]
    let inline zero() : ^Vec =
        ( getImpl VectorBuiltin
            (Unchecked.defaultof<VectorClass< ^a, ^Vec >>)
        ).Zero()

    [<CompiledName "FromScalar">]
    let inline fromScalar(a : ^a) : ^Vec =
        ( getImpl VectorBuiltin
            (Unchecked.defaultof<VectorClass< ^a, ^Vec >>)
        ).FromScalar a

    [<CompiledName "Dot">]
    let inline dot (a : ^Vec) (b : ^Vec) : ^a =
        ( getImpl VectorBuiltin
            (Unchecked.defaultof<VectorClass< ^a, ^Vec >>)
        ).Dot a b

    [<CompiledName "SquaredLength">]
    let inline squaredLength (v : ^Vec) : ^a =
        dot v v

    [<CompiledName "Length">]
    let inline length (v : ^Vec) : ^b =
        sqrt (squaredLength v)

    [<CompiledName "Normalize">]
    let inline normalize (v : ^Vec) : ^Vec =
        v / fromScalar (length v)