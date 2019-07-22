namespace wraikny.Tart.Helper.Geometry

open wraikny.Tart.Helper.Math
open wraikny.Tart.Helper.Extension

open FSharpPlus

[<Struct>]
type Sphere< 'a, 'Vec> =
    {
        radius : 'a
        center : 'Vec
    }

    static member Init center radius = {
        radius = radius
        center = center
    }

    /// Applicative
    static member inline Return (k : 't) = Sphere<'t, _>.Init (pure' k) k
    static member (<*>) (f, x : Sphere<_, _>) = {
        radius = f.radius x.radius
        center = f.center x.center
    }

    // --------------------------------------------------------------------------------

    static member inline Zero (_ : Sphere< 'a, 'Vec>, _) = Sphere< 'a, 'Vec>.Return zero

    static member inline One (_ : Sphere< 'a, 'Vec>, _) = Sphere< 'a, 'Vec>.Return one


type ^a Sphere2 = Sphere< ^a, ^a Vec2 >
type ^a Sphere3 = Sphere< ^a, ^a Vec3 >
type ^a Sphere4 = Sphere< ^a, ^a Vec4 >


[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Sphere =
    [<CompiledName "Init">]
    let init (c : '``Vec<'a>``) (r : 'a) = Sphere< 'a, '``Vec<'a>`` >.Init c r

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