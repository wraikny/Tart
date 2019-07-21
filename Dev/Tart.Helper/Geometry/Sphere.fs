namespace wraikny.Tart.Helper.Geometry

open wraikny.Tart.Helper.Math

open FSharpPlus

[<Struct>]
type Sphere< 'a, 'Vec> =
    {
        center : 'Vec
        radius : 'a
    }

    static member inline Map((s : Sphere< _, _>, f : 'T -> 'U), _mthd : FSharpPlus.Control.Map) =
        { center = map f s.center; radius = f s.radius }


type ^a Sphere2 = Sphere< ^a, ^a Vec2 >
type ^a Sphere3 = Sphere< ^a, ^a Vec3 >
type ^a Sphere4 = Sphere< ^a, ^a Vec4 >


[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Sphere =
    [<CompiledName "Init">]
    let inline init(c, r) =
        { center = c; radius = r; }

    [<CompiledName "Zero">]
    let inline zero() =
        let zero : ^a = LanguagePrimitives.GenericZero
        init(Vector.zero(), zero)

    [<CompiledName "Center">]
    let inline center c = c.center

    [<CompiledName "Radius">]
    let inline radius c = c.radius

    [<CompiledName "IsInside">]
    let inline isInside p c : bool =
        let distance = Vector.squaredLength(p - c.center)
        distance < (c.radius * c.radius)

    [<CompiledName "IsCollided">]
    let inline isCollided a b : bool =
        let distance = (a.center - b.center) |> Vector.squaredLength
        let radiusSum =
            let x = (a.radius + b.radius)
            x * x

        distance <= radiusSum