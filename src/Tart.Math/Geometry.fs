namespace wraikny.Tart.Math

open FSharpPlus
//open FSharpPlus.Math.Applicative

[<Struct>]
type 'Vec Line = {
    startPoint : 'Vec
    endPoint : 'Vec
} with

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


[<RequireQualifiedAccess; CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Line =
    let inline init s e = Line<_>.Init s e

    let inline startPoint l = l.startPoint

    let inline endPoint l = l.endPoint

    let inline equal a b =
        (a.startPoint = b.startPoint && a.endPoint = b.endPoint)
        || (a.endPoint = b.startPoint && a.startPoint = b.endPoint)

    let inline length (l : '``Vec<'a>`` Line) =
        Vector.length(l.startPoint - l.endPoint)

    let inline squaredLength (l : '``Vec<'a>`` Line) =
        Vector.squaredLength(l.startPoint - l.endPoint)


type 'Vec Rect = {
    position : 'Vec
    size : 'Vec
} with

    static member inline Init position size = {
        position = position
        size = size
    }

    /// Applicative
    static member inline Return (k : 't) = Rect< 't >.Init k k
    static member inline (<*>) (f, x : _ Rect) = {
        position = f.position x.position
        size = f.size x.size
    }

    // --------------------------------------------------------------------------------

    static member inline Zero (_ : 'T Rect, _) = Rect<'T>.Return zero

    static member inline One (_ : 'T Rect, _) = Rect<'T>.Return one


type ^a Rect2 = ^a Vec2 Rect
type ^a Rect3 = ^a Vec3 Rect
type ^a Rect4 = ^a Vec4 Rect


[<RequireQualifiedAccess; CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Rect =
    open FSharpPlus.Math.Applicative

    let inline init position size = Rect<_>.Init position size

    let inline position r = r.position

    let inline size r = r.size

    let inline left r = r.position.x

    let inline right r = r.position.x + r.size.x

    let inline up r = r.position.y

    let inline down r = r.position.y + r.size.y

    let inline diagonalPosition r : '``Vec<'a>`` =
        Vector.constraint' (Unchecked.defaultof< '``Vec<'a>`` >)
        r.position + r.size

    let inline centerPosition r : '``Vec<'a>`` =
        Vector.constraint' (Unchecked.defaultof< '``Vec<'a>`` >)
        let two = one + one
        r.position + r.size / two

    let inline get_LU_RD r : ('``Vec<'a>`` * '``Vec<'a>``) =
        Vector.constraint' (Unchecked.defaultof< '``Vec<'a>`` >)
        r.position, diagonalPosition r

    let inline isCollidedAxis(axis : '``Vec<'a>`` -> 'a) (aLU, aRD) (bLU, bRD) : bool =
        Utils.inCollision (axis aLU, axis aRD) (axis bLU, axis bRD)

    let inline isInside (p : '``Vec<'a>``) r : bool =
        Vector.constraint' (Unchecked.defaultof< '``Vec<'a>`` >)

        let lu, rd = get_LU_RD r
        
        zip (toSeq lu) (toSeq rd)
        |> zip (toSeq p)
        |>> fun (p', (lu', rd')) ->
            lu' <= p' && p' <= rd'
        |> fold (&&) true


    let inline isCollided2 (a : ^a Rect2) (b : ^a Rect2) : bool =
        let aLU = a.position
        let aRD = aLU .+. a.size

        let bLU = b.position
        let bRD = bLU .+. b.size

        let inline f (axis : _ -> ^a) =
             not(axis bRD < axis aLU || axis aRD < axis bLU)

        (f Vector.x) && (f Vector.y)

[<Struct>]
type Sphere< 'a, 'Vec> = {
    radius : 'a
    center : 'Vec
} with

    static member inline Init center radius = {
        radius = radius
        center = center
    }

    /// Applicative
    static member inline Return (k : 't) = Sphere<'t, _>.Init (pure' k) k
    static member inline (<*>) (f, x : Sphere<_, _>) = {
        radius = f.radius x.radius
        center = f.center x.center
    }

    // --------------------------------------------------------------------------------

    static member inline Zero (_ : Sphere< 'a, 'Vec>, _) = Sphere< 'a, 'Vec>.Return zero

    static member inline One (_ : Sphere< 'a, 'Vec>, _) = Sphere< 'a, 'Vec>.Return one


type ^a Sphere2 = Sphere< ^a, ^a Vec2 >
type ^a Sphere3 = Sphere< ^a, ^a Vec3 >
type ^a Sphere4 = Sphere< ^a, ^a Vec4 >


[<RequireQualifiedAccess; CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Sphere =
    let init (c : '``Vec<'a>``) (r : 'a) = Sphere< 'a, '``Vec<'a>`` >.Init c r

    let inline center c = c.center

    let inline radius c = c.radius

    let inline isInside (p : 'a) (c : Sphere<'a, '``Vec<'a>``>) : bool =
        Vector.constraint' (Unchecked.defaultof< '``Vec<'a>`` >)

        let distance = Vector.squaredLength(p - c.center)
        distance < (c.radius * c.radius)

    let inline isCollided (a : Sphere<'a, '``Vec<'a>``>) (b : Sphere<'a, '``Vec<'a>``>) : bool =
        Vector.constraint' (Unchecked.defaultof< '``Vec<'a>`` >)

        let distance = (a.center - b.center) |> Vector.squaredLength
        let radiusSum =
            let x = (a.radius + b.radius)
            x * x

        distance <= radiusSum

[<Struct>]
type Triangle< 'a > = {
    p1 : 'a
    p2 : 'a
    p3 : 'a
} with

    static member inline Init p1 p2 p3 = {
        p1 = p1
        p2 = p2
        p3 = p3
    }

    /// Applicative
    static member inline Return (k : 't) = Triangle< 't >.Init k k k
    static member inline (<*>) (f, x : _ Triangle) = {
        p1 = f.p1 x.p1
        p2 = f.p2 x.p2
        p3 = f.p3 x.p3
    }

    // --------------------------------------------------------------------------------

    static member inline Zero (_ : 'T Triangle, _) = Triangle<'T>.Return zero

    static member inline One (_ : 'T Triangle, _) = Triangle<'T>.Return one


type ^a Triangle2 = ^a Vec2 Triangle
type ^a Triangle3 = ^a Vec3 Triangle
type ^a Triangle4 = ^a Vec4 Triangle


[<RequireQualifiedAccess; CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Triangle =
    let inline init p1 p2 p3 = Triangle<_>.Init p1 p2 p3

    let inline p1 t = t.p1
    let inline p2 t = t.p2
    let inline p3 t = t.p3

    let inline equal a b =
        let inline f q1 q2 q3 =
            (a.p1 = q1 b) && (a.p2 = q2 b) && (a.p3 = q3 b)

        (f p1 p2 p3) || (f p2 p3 p1) || (f p3 p1 p2) ||
        (f p1 p3 p2) || (f p2 p1 p3) || (f p3 p2 p1)

    let inline hasCommonPoint a b =
        seq {
            let l = [p1; p2; p3]
            for s in l do
            for t in l do
                yield (s, t)
        }
        |> Seq.exists(fun (s, t) -> s a = t b)

    let circumscribedCircle (t : float32 Triangle2) : float32 Sphere2 =
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

        let center = Vec2.init x y

        Vec2.init (center.x - x1) (center.y - y1)
        |> Vector.length
        |> Sphere.init center
