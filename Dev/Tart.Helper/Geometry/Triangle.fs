namespace wraikny.Tart.Helper.Geometry

open wraikny.Tart.Helper.Math

[<Struct>]
type Triangle< 'a > =
    {
        p1 : 'a
        p2 : 'a
        p3 : 'a
    }


type ^a Triangle2 = ^a Vec2 Triangle
type ^a Triangle3 = ^a Vec3 Triangle
type ^a Triangle4 = ^a Vec4 Triangle


[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Triangle =
    [<CompiledName "Init">]
    let inline init(p1, p2, p3) : Triangle<_> =
        { p1 = p1; p2 = p2; p3 = p3 }

    [<CompiledName "Zero">]
    let inline zero() : Triangle< ^a> =
        let zero = LanguagePrimitives.GenericZero
        init(zero, zero, zero)

    [<CompiledName "P1">]
    let inline p1 t = t.p1

    [<CompiledName "P2">]
    let inline p2 t = t.p2

    [<CompiledName "P3">]
    let inline p3 t = t.p3

    [<CompiledName "Equal">]
    let inline equal a b =
        let f q1 q2 q3 =
            (a.p1 = q1 b) && (a.p2 = q2 b) && (a.p3 = q3 b)

        (f p1 p2 p3) || (f p2 p3 p1) || (f p3 p1 p2) ||
        (f p1 p3 p2) || (f p2 p1 p3) || (f p3 p2 p1)

    [<CompiledName "HasCommonPoint">]
    let inline hasCommonPoint a b =
        seq {
            let l = [p1; p2; p3]
            for s in l do
            for t in l do
            yield (s, t)
        }
        |> Seq.map(fun (s, t) ->
            (s a = t b)
        )
        |> Seq.fold (||) false


    [<CompiledName "CircumscribedCircle">]
    let inline circumscribedCircle (t : Triangle< float32 Vec2 >) : Sphere< float32, float32 Vec2 > =
        let x1, y1 = t.p1.x, t.p1.y
        let x2, y2 = t.p2.x, t.p2.y
        let x3, y3 = t.p3.x, t.p3.y
        let c = 2.0f * ((x2 - x1) * (y3 - y1) - (y2 - y1) * (x3 - x1))

        let x =
            ( (y3 - y1) * (x2 * x2 - x1 * x1 + y2 * y2 - y1 * y1)
            + (y1 - y2) * (x3 * x3 - x1 * x1 + y3 * y3 - y1 * y1)) / c

        let y = 
            ( (x1 - x3) * (x2 * x2 - x1 * x1 + y2 * y2 - y1 * y1)
            + (x2 - x1) * (x3 * x3 - x1 * x1 + y3 * y3 - y1 * y1)) / c

        let center = Vec2.init(x, y)

        let r = VectorClass.length <| Vec2.init(center.x - x1, center.y - y1)

        Sphere.init(center, r)
