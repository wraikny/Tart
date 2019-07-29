namespace wraikny.Tart.Helper.Geometry

open wraikny.Tart.Helper.Math

open FSharpPlus

[<Struct>]
type 'Vec Line =
    {
        startPoint : 'Vec
        endPoint : 'Vec
    }

    static member inline Init startPoint endPoint = {
        startPoint = startPoint
        endPoint = endPoint
    }

    /// Applicative
    static member inline Return (k : ^t) = Line< ^t >.Init k k
    static member inline (<*>) (f, x : _ Line) = {
        startPoint = f.startPoint x.startPoint
        endPoint = f.endPoint x.endPoint
    }

    // --------------------------------------------------------------------------------

    static member inline Zero (_ : 'T Line, _) = Line<'T>.Return zero
    
    static member inline One (_ : 'T Line, _) = Line<'T>.Return one


type ^a Line2 = ^a Vec2 Line
type ^a Line3 = ^a Vec3 Line
type ^a Line4 = ^a Vec4 Line


[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Line =
    [<CompiledName "Init">]
    let inline init s e = Line<_>.Init s e

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