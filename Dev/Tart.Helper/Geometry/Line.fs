namespace wraikny.Tart.Helper.Geometry

open wraikny.Tart.Helper.Math

[<Struct>]
type 'Vec Line=
    {
        startPoint : 'Vec
        endPoint : 'Vec
    }

    static member inline Map((l: _ Line, f : 'T -> 'U), _mthd : FSharpPlus.Control.Map) =
        {startPoint = f l.startPoint; endPoint = f l.endPoint }


type ^a Line2 = ^a Vec2 Line
type ^a Line3 = ^a Vec3 Line
type ^a Line4 = ^a Vec4 Line


[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Line =
    [<CompiledName "Init">]
    let inline init(s, e) =
        { startPoint = s; endPoint = e; }

    [<CompiledName "Zero">]
    let inline zero() =
        let zero = Vector.zero()
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
        Vector.length(l.startPoint - l.endPoint)

    [<CompiledName "SquaredLength">]
    let inline squaredLength l =
        Vector.squaredLength(l.startPoint - l.endPoint)