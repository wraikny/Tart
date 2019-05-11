namespace wraikny.Tart.Helper.Geometry

open wraikny.Tart.Helper.Math

[<Struct>]
type Line< ^a, ^Vec
        when (VectorBuiltin or ^Vec) :
            (static member VectorImpl : ^Vec -> VectorClass< ^a, ^Vec >)
    > =
    {
        startPoint : ^Vec
        endPoint : ^Vec
    }


[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Line =
    [<CompiledName "Init">]
    let inline init(s, e) : Line<_, _> =
        { startPoint = s; endPoint = e; }

    [<CompiledName "Zero">]
    let inline zero() : Line<_, _> =
        let zero = VectorClass.zero()
        init(zero, zero)

    [<CompiledName "StartPoint">]
    let inline startPoint l = l.startPoint

    [<CompiledName "EndPoint">]
    let inline endPoint l = l.endPoint

    [<CompiledName "Equal">]
    let inline equal a b =
        (a.startPoint = b.startPoint && a.endPoint = b.endPoint)
        || (a.endPoint = b.startPoint && a.startPoint = b.endPoint)

    [<CompiledName "Length">]
    let inline length l =
        VectorClass.length(l.startPoint - l.endPoint)

    [<CompiledName "SquaredLength">]
    let inline squaredLength l =
        VectorClass.squaredLength(l.startPoint - l.endPoint)