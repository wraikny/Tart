namespace wraikny.Tart.Helper.Math


type VectorClass< ^a, ^Vec > = {
    Zero : unit -> ^Vec
    Dot : ^Vec -> ^Vec -> ^a
    SquaredLength : ^Vec -> ^a
    Length : ^Vec -> ^a
    Normalize : ^Vec -> ^Vec
}


type VectorBuiltin = VectorBuiltin with
    static member inline VectorImpl(_ : ^a Vec2) : VectorClass< ^a, ^a Vec2 >
        when (^a or ^a Vec2) : (static member (*) : ^a Vec2 * ^a -> ^a Vec2)
        =
        {
            Zero = Vec2.zero
            Dot = Vec2.dot
            SquaredLength = Vec2.squaredLength
            Length = Vec2.length
            Normalize = Vec2.normalize
        }
    
    static member inline VectorImpl(_ : ^a Vec3): VectorClass< ^a, ^a Vec3 >
        when (^a or ^a Vec3) : (static member (*) : ^a Vec3 * ^a -> ^a Vec3)
        =
        {
            Zero = Vec3.zero
            Dot = Vec3.dot
            SquaredLength = Vec3.squaredLength
            Length = Vec3.length
            Normalize = Vec3.normalize
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

    [<CompiledName "Dot">]
    let inline dot (a : ^Vec) (b : ^Vec) : ^a =
        ( getImpl VectorBuiltin
            (Unchecked.defaultof<VectorClass< ^a, ^Vec >>)
        ).Dot a b

    [<CompiledName "SquaredLength">]
    let inline squaredLength (v : ^Vec) : ^a =
        ( getImpl VectorBuiltin
            (Unchecked.defaultof<VectorClass< ^a, ^Vec >>)
        ).SquaredLength v

    [<CompiledName "Length">]
    let inline length (v : ^Vec) : ^b =
        ( getImpl VectorBuiltin
            (Unchecked.defaultof<VectorClass< _, ^Vec >>)
        ).Length v

    [<CompiledName "Normalize">]
    let inline normalize (v : ^Vec) : ^Vec =
        ( getImpl VectorBuiltin
            (Unchecked.defaultof<VectorClass< _, ^Vec >>)
        ).Normalize v