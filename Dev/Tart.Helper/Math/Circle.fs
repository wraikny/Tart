namespace wraikny.Tart.Helper.Math


type Circle< ^a, ^Vec
        when (VectorBuiltin or ^Vec) : (static member VectorImpl : ^Vec -> VectorClass< ^a, ^Vec >)
    > =
    {
        center : ^Vec
        radius : ^a
    }


[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Circle =
    [<CompiledName "Init">]
    let inline init(c, r) : Circle<_, _> =
        { center = c; radius = r; }

    [<CompiledName "Zero">]
    let inline zero() : Circle<_, _> =
        let zero : ^a = LanguagePrimitives.GenericZero
        init(Vec2.zero(), zero)

    [<CompiledName "Center">]
    let inline center c = c.center

    [<CompiledName "Radius">]
    let inline radius c = c.radius