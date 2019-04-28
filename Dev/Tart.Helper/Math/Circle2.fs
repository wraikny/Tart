namespace wraikny.Tart.Helper.Math

type ^a Circle2 =
    {
        center : ^a Vec2
        radius : ^a
    }


[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Circle2 =
    [<CompiledName "Init">]
    let inline init(c, r) : ^a Circle2 =
        { center = c; radius = r; }

    [<CompiledName "Zero">]
    let inline zero() : ^a Circle2 =
        let zero : ^a = LanguagePrimitives.GenericZero
        init(Vec2.zero(), zero)

    [<CompiledName "Center">]
    let inline center c = c.center

    [<CompiledName "Radius">]
    let inline radius c = c.radius