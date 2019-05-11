namespace wraikny.Tart.Helper.Geometry

open wraikny.Tart.Helper.Math

[<Struct>]
type Circle< ^a, ^Vec
        when (VectorBuiltin or ^Vec) :
            (static member VectorImpl : ^Vec -> VectorClass< ^a, ^Vec >)
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

    [<CompiledName "IsInside">]
    let inline isInside p c =
        let distance = VectorClass.squaredLength(p - c.center)
        distance < (c.radius * c.radius)

    [<CompiledName "IsCollided">]
    let inline isCollided (a : Circle< ^a, ^Vec >) (b : Circle< ^a, ^Vec >) : bool =
        let distance = (a.center - b.center) |> VectorClass.squaredLength
        let radiusSum =
            let x = (a.radius + b.radius)
            x * x

        distance <= radiusSum