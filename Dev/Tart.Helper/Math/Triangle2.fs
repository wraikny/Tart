namespace wraikny.Tart.Helper.Math

type ^a Triangle2 =
    {
        p1 : ^a Vec2
        p2 : ^a Vec2
        p3 : ^a Vec2
    }



[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Triangle2 =
    [<CompiledName "Init">]
    let inline init(p1, p2, p3) : ^a Triangle2 =
        { p1 = p1; p2 = p2; p3 = p3 }

    [<CompiledName "Zero">]
    let inline zero() : ^a Triangle2 =
        let zero = Vec2.zero()
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
            s a = s b
        )
        |> Seq.fold (||) false


    let inline circumscribedCircle (t : ^a Triangle2) : ^a Circle2 =
        let x1, y1 = t.p1.x, t.p1.y
        let x2, y2 = t.p2.x, t.p2.y
        let x3, y3 = t.p3.x, t.p3.y

        let one : ^a = LanguagePrimitives.GenericOne
        let two : ^a = one + one

        let c : ^a = (two * ((x2 - x1) * (y3 - y1) - (y2 - y1) * (x3 - x1))) ** -one

        let x : ^a =
            ( (y3 - y1) * (x2 * x2 - x1 * x1 + y2 * y2 - y1 * y1)
            + (y1 - y2) * (x3 * x3 - x1 * x1 + y3 * y3 - y1 * y1)) * c

        let y : ^a = 
            ( (x1 - x3) * (x2 * x2 - x1 * x1 + y2 * y2 - y1 * y1)
            + (x2 - x1) * (x3 * x3 - x1 * x1 + y3 * y3 - y1 * y1)) * c

        let center : ^a Vec2 = Vec2.init(x, y)

        let r = Vec2.length <| Vec2.init(center.x - x1, center.y - y1)

        Circle2.init(center, r)