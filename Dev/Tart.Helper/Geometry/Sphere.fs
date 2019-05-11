namespace wraikny.Tart.Helper.Geometry

open wraikny.Tart.Helper.Math

[<Struct>]
type Sphere< ^a, ^Vec> =
    {
        center : ^Vec
        radius : ^a
    }


[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Sphere =
    [<CompiledName "Init">]
    let inline init(c, r) =
        { center = c; radius = r; }

    [<CompiledName "Zero">]
    let inline zero() =
        let zero : ^a = LanguagePrimitives.GenericZero
        init(VectorClass.zero(), zero)

    [<CompiledName "Center">]
    let inline center c = c.center

    [<CompiledName "Radius">]
    let inline radius c = c.radius

    [<CompiledName "IsInside">]
    let inline isInside p c : bool
        when (VectorBuiltin or ^Vec) :
            (static member VectorImpl : ^Vec -> VectorClass< ^a, ^Vec >)
        =
        let distance = VectorClass.squaredLength(p - c.center)
        distance < (c.radius * c.radius)

    [<CompiledName "IsCollided">]
    let inline isCollided a b : bool
        when (VectorBuiltin or ^Vec) :
            (static member VectorImpl : ^Vec -> VectorClass< ^a, ^Vec >)
        =
        let distance = (a.center - b.center) |> VectorClass.squaredLength
        let radiusSum =
            let x = (a.radius + b.radius)
            x * x

        distance <= radiusSum