module wraikny.Tart.Helper.Math

open FSharpPlus

let [<Literal>] Pi = 3.14159265359f

module Angle =
    let inline degreeToRadian degree =
        degree * Pi / 180.0f

    let inline radianToDegree radian =
        radian * 180.0f / Pi


module Utils =
    let inline inCollision (aLeft : 'a, aRight : 'a) (bLeft, bRight) =
        not (aRight < bLeft || bRight < aLeft)

    //let inline normalDistribution (mean : float32) (sd : float32) (x : float32) =
    //    exp(-(x - mean) * (x - mean) / 2.0f / sd / sd ) / 2.50662827463f / sd

    let inline boxMullersMethod u1 u2 =
        let a = sqrt(-2.0f * log u1)
        let x = 2.0f * Pi * u2
        a * cos x, a * sin x


module BinarySearch =
    let inline binarySearch count predicate (current : 'a) (target : 'a) : 'a =
        let two : 'a = one + one

        let rec search count diffSum current target =
            if count <= 0 then diffSum
            else
                let middle = (current + target) / two
                let newDiffSum = diffSum + (middle - current)

                if predicate newDiffSum then
                    search (count - 1) newDiffSum middle target
                else
                    search (count - 1) diffSum current middle

        search count (zero : 'a) current target


open FSharpPlus.Math.Applicative

[<Struct>]
type ^a Vec2 = {
    x : ^a
    y : ^a
} with
    static member inline Init x y = { x = x; y = y }

    /// Foldable
    static member inline ToSeq (v : 't Vec2) =
        seq { yield v.x; yield v.y }

    /// Applicative
    static member inline Return (k : 't) = Vec2<'t>.Init k k
    static member inline (<*>) (f, v : 't Vec2) = Vec2<'t>.Init (f.x v.x) (f.y v.y)

    // --------------------------------------------------------------------------------

    static member inline Zero (_ : 't Vec2, _) = Vec2<'t>.Return zero

    static member inline One (_ : 't Vec2, _) = Vec2<'t>.Return one

    static member inline Abs (v : _ Vec2) = abs <!> v

    static member inline (~-) (v) = -.v
    static member inline (+) (a, b) = a .+. b
    static member inline (-) (a, b) = a .-. b
    static member inline (*) (a, b) = a .*. b
    static member inline (/) (a, b) = a ./. b
    static member inline ( .* ) (a, b) = a .* b
    static member inline ( *. ) (a, b) = a *. b
    static member inline ( ./ ) (a, b) = a ./ b
    static member inline ( /. ) (a, b) = a /. b

    static member inline Dot (a : ^t Vec2, b : ^t Vec2) : ^t =
        a.x * b.x + a.y * b.y


[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Vec2 =
    let inline init x y : 'a Vec2 = Vec2<_>.Init x y

    let inline angle(v : ^a Vec2) : ^a =
        atan2 v.y v.x

    let inline fromAngle angle =
        Vec2<_>.Init (cos angle) (sin angle)


[<Struct>]
type ^a Vec3 = {
    x : ^a
    y : ^a
    z : ^a
} with
    static member inline Init x y z = { x = x; y = y; z = z }
    
    /// Foldable
    static member inline ToSeq (v : 't Vec3) =
        seq {
            yield v.x;
            yield v.y;
            yield v.z;
        }
    
    /// Applicative
    static member inline Return (k : 't) = Vec3<'t>.Init k k k
    static member inline (<*>) (f, v : 't Vec3) = Vec3<'t>.Init (f.x v.x) (f.y v.y) (f.z v.z)
    
    // --------------------------------------------------------------------------------
    
    static member inline Zero (_ : 't Vec3, _) = Vec3<'t>.Return zero
    
    static member inline One (_ : 't Vec3, _) = Vec3<'t>.Return one
    
    static member inline Abs (v : _ Vec3) = abs <!> v
    
    static member inline (~-) (v) = -.v
    static member inline (+) (a, b) = a .+. b
    static member inline (-) (a, b) = a .-. b
    static member inline (*) (a, b) = a .*. b
    static member inline (/) (a, b) = a ./. b
    static member inline ( .* ) (a, b) = a .* b
    static member inline ( *. ) (a, b) = a *. b
    static member inline ( ./ ) (a, b) = a ./ b
    static member inline ( /. ) (a, b) = a /. b

    static member inline Dot (a : ^t Vec3, b : ^t Vec3) : ^t =
        a.x * b.x + a.y * b.y + a.z * b.z


[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Vec3 =
    let inline init x y z : 'a Vec3 = Vec3<_>.Init x y z

    let inline cross
        ({ x = x1; y = y1; z = z1 } : 'a Vec3)
        ({ x = x2; y = y2; z = z2 } : 'a Vec3)
        : 'a Vec3 =
        Vec3<_>.Init
            (y1 * z2 - z1 * y2)
            (z1 * x2 - x1 * z2)
            (x1 * y2 - y1 * x2)

[<Struct>]
type ^a Vec4 = {
    x : ^a
    y : ^a
    z : ^a
    w : ^a
} with
    
    static member inline Init x y z w = { x = x; y = y; z = z; w = w }

    /// Foldable
    static member inline ToSeq (v : 't Vec4) =
        seq {
            yield v.x;
            yield v.y;
            yield v.z;
            yield v.w;
        }

    /// Applicative
    static member inline Return (k : 't) = Vec4<'t>.Init k k k k
    static member inline (<*>) (f, v : 't Vec4) =
        Vec4<'t>.Init (f.x v.x) (f.y v.y) (f.z v.z) (f.w v.w)

    // --------------------------------------------------------------------------------

    static member inline Zero (_ : 't Vec4, _) = Vec4<'t>.Return zero

    static member inline One (_ : 't Vec4, _) = Vec4<'t>.Return one

    static member inline Abs (v : _ Vec4) = abs <!> v

    static member inline (~-) (v) = -.v
    static member inline (+) (a, b) = a .+. b
    static member inline (-) (a, b) = a .-. b
    static member inline (*) (a, b) = a .*. b
    static member inline (/) (a, b) = a ./. b
    static member inline ( .* ) (a, b) = a .* b
    static member inline ( *. ) (a, b) = a *. b
    static member inline ( ./ ) (a, b) = a ./ b
    static member inline ( /. ) (a, b) = a /. b

    static member inline Dot (a : ^t Vec4, b : ^t Vec4) : ^t =
        a.x * b.x + a.y * b.y + a.z * b.z + a.w * b.w


module Vec4 =
    let inline init x y z w : 'a Vec4 = Vec4<_>.Init x y z w

#nowarn "0064"

[<Struct>]
type Vector< 'a, '``Vec<'a>``> = Vector of 'a * '``Vec<'a>``


type VectorBuiltin = VectorBuiltin with
    static member inline VectorImpl(v : 'a Vec2) = Vector(v.x, v)
    static member inline VectorImpl(v : 'a Vec3) = Vector(v.x, v)
    static member inline VectorImpl(v : 'a Vec4) = Vector(v.x, v)


[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Vector =
    let inline private getImpl v =
        let inline getImpl' (_ : ^Builtin) (_ : Vector< ^a, ^Va > ) =
            ((^Builtin or ^Va) : (static member VectorImpl : ^Va -> Vector< ^a, ^Va >)
                Unchecked.defaultof<_>
            )
        getImpl' VectorBuiltin v

    let inline constraint' v = getImpl v |> ignore


    let inline zero() : ^``Vec<'a>`` =
        constraint' (Unchecked.defaultof<Vector< ^a, ^``Vec<'a>`` >>)
        zero


    let inline one() : ^``Vec<'a>`` =
        constraint' (Unchecked.defaultof<Vector< ^a, ^``Vec<'a>`` >>)
        one


    let inline axes() : (^``Vec<'a>`` -> ^a) list =
        constraint' (Unchecked.defaultof<Vector< ^a, ^``Vec<'a>`` >>)

        (^``Vec<'a>`` : (static member Axes : unit -> (^``Vec<'a>`` -> ^a) list) ())
        

    let inline dot (a : ^``Vec<'a>``) (b : ^``Vec<'a>``) : ^a =
        constraint' (Unchecked.defaultof<Vector< ^a, ^``Vec<'a>`` >>)
        (^``Vec<'a>`` : (static member inline Dot :_*_->_) (a, b))


    let inline squaredLength (v : ^``Vec<'a>``) : ^a =
        dot v v


    let inline length (v : ^``Vec<'a>``) : ^a =
        FSharp.Core.Operators.sqrt (squaredLength v)


    let inline normalize (v : ^``Vec<'a>``) : ^``Vec<'a>`` =
        let len = length v
        if len = FSharpPlus.Operators.zero then
            zero()
        else
            v ./ len

    // -----------------------------------------

    let inline x (a : ^``Vec<'a>``) : ^a =
        constraint' (Unchecked.defaultof<Vector< ^a, ^``Vec<'a>`` >>)
        (^``Vec<'a>`` : (member x : ^a) a)

    let inline y (a : ^``Vec<'a>``) : ^a =
        constraint' (Unchecked.defaultof<Vector< ^a, ^``Vec<'a>`` >>)
        (^``Vec<'a>`` : (member y : ^a) a)

    let inline z (a : ^``Vec<'a>``) : ^a =
        constraint' (Unchecked.defaultof<Vector< ^a, ^``Vec<'a>`` >>)
        (^``Vec<'a>`` : (member z : ^a) a)

    let inline w (a : ^``Vec<'a>``) : ^a =
        constraint' (Unchecked.defaultof<Vector< ^a, ^``Vec<'a>`` >>)
        (^``Vec<'a>`` : (member w : ^a) a)

    // -----------------------------------------

    let inline private kk (v : '``Vec<'a>``) k1 k2 : 'a Vec2 =
        constraint' (Unchecked.defaultof<Vector< 'a, '``Vec<'a>`` >>)
        Vec2.init (k1 v) (k2 v)

    let inline xx (v : '``Vec<'a>``) = kk v x x
    let inline xy (v : '``Vec<'a>``) = kk v x y
    let inline xz (v : '``Vec<'a>``) = kk v x z
    let inline xw (v : '``Vec<'a>``) = kk v x w
    
    let inline yx (v : '``Vec<'a>``) = kk v y x
    let inline yy (v : '``Vec<'a>``) = kk v y y
    let inline yz (v : '``Vec<'a>``) = kk v y z
    let inline yw (v : '``Vec<'a>``) = kk v y w
    
    let inline zx (v : '``Vec<'a>``) = kk v z x
    let inline zy (v : '``Vec<'a>``) = kk v z y
    let inline zz (v : '``Vec<'a>``) = kk v z z
    let inline zw (v : '``Vec<'a>``) = kk v z w
    
    let inline wx (v : '``Vec<'a>``) = kk v w x
    let inline wy (v : '``Vec<'a>``) = kk v w y
    let inline wz (v : '``Vec<'a>``) = kk v w z
    let inline ww (v : '``Vec<'a>``) = kk v w w

    // -----------------------------------------

    let inline private kkk (v : '``Vec<'a>``) k1 k2 k3 : 'a Vec3 =
        constraint' (Unchecked.defaultof<Vector< 'a, '``Vec<'a>`` >>)
        Vec3.init (k1 v) (k2 v) (k3 v)

    
    let inline xxx (v : '``Vec<'a>``) = kkk v x x x
    let inline xxy (v : '``Vec<'a>``) = kkk v x x y
    let inline xxz (v : '``Vec<'a>``) = kkk v x x z
    let inline xxw (v : '``Vec<'a>``) = kkk v x x w
    
    let inline xyx (v : '``Vec<'a>``) = kkk v x y x
    let inline xyy (v : '``Vec<'a>``) = kkk v x y y
    let inline xyz (v : '``Vec<'a>``) = kkk v x y z
    let inline xyw (v : '``Vec<'a>``) = kkk v x y w
    
    let inline xzx (v : '``Vec<'a>``) = kkk v x z x
    let inline xzy (v : '``Vec<'a>``) = kkk v x z y
    let inline xzz (v : '``Vec<'a>``) = kkk v x z z
    let inline xzw (v : '``Vec<'a>``) = kkk v x z w
    
    let inline xwx (v : '``Vec<'a>``) = kkk v x w x
    let inline xwy (v : '``Vec<'a>``) = kkk v x w y
    let inline xwz (v : '``Vec<'a>``) = kkk v x w z
    let inline xww (v : '``Vec<'a>``) = kkk v x w w
    
    
    let inline yxx (v : '``Vec<'a>``) = kkk v y x x
    let inline yxy (v : '``Vec<'a>``) = kkk v y x y
    let inline yxz (v : '``Vec<'a>``) = kkk v y x z
    let inline yxw (v : '``Vec<'a>``) = kkk v y x w
    
    let inline yyx (v : '``Vec<'a>``) = kkk v y y x
    let inline yyy (v : '``Vec<'a>``) = kkk v y y y
    let inline yyz (v : '``Vec<'a>``) = kkk v y y z
    let inline yyw (v : '``Vec<'a>``) = kkk v y y w
    
    let inline yzx (v : '``Vec<'a>``) = kkk v y z x
    let inline yzy (v : '``Vec<'a>``) = kkk v y z y
    let inline yzz (v : '``Vec<'a>``) = kkk v y z z
    let inline yzw (v : '``Vec<'a>``) = kkk v y z w
    
    let inline ywx (v : '``Vec<'a>``) = kkk v y w x
    let inline ywy (v : '``Vec<'a>``) = kkk v y w y
    let inline ywz (v : '``Vec<'a>``) = kkk v y w z
    let inline yww (v : '``Vec<'a>``) = kkk v y w w
    
    
    let inline zxx (v : '``Vec<'a>``) = kkk v z x x
    let inline zxy (v : '``Vec<'a>``) = kkk v z x y
    let inline zxz (v : '``Vec<'a>``) = kkk v z x z
    let inline zxw (v : '``Vec<'a>``) = kkk v z x w
    
    let inline zyx (v : '``Vec<'a>``) = kkk v z y x
    let inline zyy (v : '``Vec<'a>``) = kkk v z y y
    let inline zyz (v : '``Vec<'a>``) = kkk v z y z
    let inline zyw (v : '``Vec<'a>``) = kkk v z y w
    
    let inline zzx (v : '``Vec<'a>``) = kkk v z z x
    let inline zzy (v : '``Vec<'a>``) = kkk v z z y
    let inline zzz (v : '``Vec<'a>``) = kkk v z z z
    let inline zzw (v : '``Vec<'a>``) = kkk v z z w
    
    let inline zwx (v : '``Vec<'a>``) = kkk v z w x
    let inline zwy (v : '``Vec<'a>``) = kkk v z w y
    let inline zwz (v : '``Vec<'a>``) = kkk v z w z
    let inline zww (v : '``Vec<'a>``) = kkk v z w w
    
    
    let inline wxx (v : '``Vec<'a>``) = kkk v w x x
    let inline wxy (v : '``Vec<'a>``) = kkk v w x y
    let inline wxz (v : '``Vec<'a>``) = kkk v w x z
    let inline wxw (v : '``Vec<'a>``) = kkk v w x w
    
    let inline wyx (v : '``Vec<'a>``) = kkk v w y x
    let inline wyy (v : '``Vec<'a>``) = kkk v w y y
    let inline wyz (v : '``Vec<'a>``) = kkk v w y z
    let inline wyw (v : '``Vec<'a>``) = kkk v w y w
    
    let inline wzx (v : '``Vec<'a>``) = kkk v w z x
    let inline wzy (v : '``Vec<'a>``) = kkk v w z y
    let inline wzz (v : '``Vec<'a>``) = kkk v w z z
    let inline wzw (v : '``Vec<'a>``) = kkk v w z w

    let inline wwx (v : '``Vec<'a>``) = kkk v w w x
    let inline wwy (v : '``Vec<'a>``) = kkk v w w y
    let inline wwz (v : '``Vec<'a>``) = kkk v w w z
    let inline www (v : '``Vec<'a>``) = kkk v w w w

    // -----------------------------------------

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


[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
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


[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Rect =
    let inline init position size = Rect<_>.Init position size

    let inline position r = r.position

    let inline size r = r.size

    let inline left r = r.position.x

    let inline right r = r.position.x + r.size.x

    let inline up r = r.position.y

    let inline down r = r.position.y + r.size.y

    let inline diagonalPosition r : '``Vec<'a>`` =
        Vector.constraint' (Unchecked.defaultof<Vector< 'a, '``Vec<'a>`` >>)
        r.position + r.size

    let inline centerPosition r : '``Vec<'a>`` =
        Vector.constraint' (Unchecked.defaultof<Vector< 'a, '``Vec<'a>`` >>)
        let two = one + one
        r.position + r.size / two

    let inline get_LU_RD r : ('``Vec<'a>`` * '``Vec<'a>``) =
        Vector.constraint' (Unchecked.defaultof<Vector< 'a, '``Vec<'a>`` >>)
        r.position, diagonalPosition r

    let inline isCollidedAxis(axis : '``Vec<'a>`` -> 'a) (aLU, aRD) (bLU, bRD) : bool =
        Utils.inCollision (axis aLU, axis aRD) (axis bLU, axis bRD)

    let inline isInside (p : '``Vec<'a>``) r : bool =
        Vector.constraint' (Unchecked.defaultof<Vector< 'a, '``Vec<'a>`` >>)

        let lu, rd = get_LU_RD r
        
        zip (toSeq lu) (toSeq rd)
        |> zip (toSeq p)
        |>> fun (p', (lu', rd')) ->
            lu' <= p' && p' <= rd'
        |> fold (&&) true


module Rect2 =
    let inline isCollided (a : ^a Rect2) (b : ^a Rect2) : bool =
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
    let init (c : '``Vec<'a>``) (r : 'a) = Sphere< 'a, '``Vec<'a>`` >.Init c r

    let inline center c = c.center

    let inline radius c = c.radius

    let inline isInside (p : 'a) (c : Sphere<'a, '``Vec<'a>``>) : bool =
        Vector.constraint' (Unchecked.defaultof<Vector< 'a, '``Vec<'a>`` >>)

        let distance = Vector.squaredLength(p - c.center)
        distance < (c.radius * c.radius)

    let inline isCollided (a : Sphere<'a, '``Vec<'a>``>) (b : Sphere<'a, '``Vec<'a>``>) : bool =
        Vector.constraint' (Unchecked.defaultof<Vector< 'a, '``Vec<'a>`` >>)

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

    static member Init p1 p2 p3 = {
        p1 = p1
        p2 = p2
        p3 = p3
    }

    /// Applicative
    static member inline Return (k : 't) = Triangle< 't >.Init k k k
    static member (<*>) (f, x : _ Triangle) = {
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


[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Triangle =
    let inline init p1 p2 p3 = Triangle<_>.Init p1 p2 p3

    let inline p1 t = t.p1
    let inline p2 t = t.p2
    let inline p3 t = t.p3

    let equal a b =
        let f q1 q2 q3 =
            (a.p1 = q1 b) && (a.p2 = q2 b) && (a.p3 = q3 b)

        (f p1 p2 p3) || (f p2 p3 p1) || (f p3 p1 p2) ||
        (f p1 p3 p2) || (f p2 p1 p3) || (f p3 p2 p1)

    let hasCommonPoint a b =
        seq {
            let l = [p1; p2; p3]
            for s in l do
            for t in l do
            yield (s, t)
        }
        |>> fun (s, t) ->
            (s a = t b)
        |> fold (||) false

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

        let r = Vector.length <| Vec2.init (center.x - x1) (center.y - y1)

        Sphere.init center r
