namespace wraikny.Tart.Math

open FSharpPlus
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


[<RequireQualifiedAccess; CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
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


[<RequireQualifiedAccess; CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
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


[<RequireQualifiedAccess; CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Vec4 =
    let inline init x y z w : 'a Vec4 = Vec4<_>.Init x y z w

#nowarn "0064"


type Vector = Vector with
    static member inline VectorImpl(_ : 'a Vec2) = ()
    static member inline VectorImpl(_ : 'a Vec3) = ()
    static member inline VectorImpl(_ : 'a Vec4) = ()


[<RequireQualifiedAccess; CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Vector =
    let inline constraint' v =
        let inline getImpl' (_ : ^V) (_ : ^Va ) =
            ((^V or ^Va) : (static member VectorImpl : ^Va -> unit)
                Unchecked.defaultof<_>
            )

        getImpl' Vector v |> ignore


    let inline axes() : (^``Vec<'a>`` -> ^a) list =
        constraint' (Unchecked.defaultof< ^``Vec<'a>`` >)

        (^``Vec<'a>`` : (static member Axes : unit -> (^``Vec<'a>`` -> ^a) list) ())
        

    let inline dot (a : ^``Vec<'a>``) (b : ^``Vec<'a>``) =
        constraint' (Unchecked.defaultof< ^``Vec<'a>`` >)
        (^``Vec<'a>`` : (static member inline Dot :_*_->_) (a, b))


    let inline squaredLength (v : ^``Vec<'a>``) =
        dot v v


    let inline length (v : ^``Vec<'a>``) =
        FSharp.Core.Operators.sqrt (squaredLength v)


    let inline normalize (v : ^``Vec<'a>``) : ^``Vec<'a>`` =
        let len = length v
        if len = FSharpPlus.Operators.zero then
            zero
        else
            v ./ len

    // -----------------------------------------

    let inline x (a : ^``Vec<'a>``) : ^a =
        constraint' (Unchecked.defaultof< ^``Vec<'a>`` >)
        (^``Vec<'a>`` : (member x : ^a) a)

    let inline y (a : ^``Vec<'a>``) : ^a =
        constraint' (Unchecked.defaultof< ^``Vec<'a>`` >)
        (^``Vec<'a>`` : (member y : ^a) a)

    let inline z (a : ^``Vec<'a>``) : ^a =
        constraint' (Unchecked.defaultof< ^``Vec<'a>`` >)
        (^``Vec<'a>`` : (member z : ^a) a)

    let inline w (a : ^``Vec<'a>``) : ^a =
        constraint' (Unchecked.defaultof< ^``Vec<'a>`` >)
        (^``Vec<'a>`` : (member w : ^a) a)

    // -----------------------------------------

    let inline private kk (v : '``Vec<'a>``) k1 k2 : 'a Vec2 =
        constraint' (Unchecked.defaultof< ^``Vec<'a>`` >)
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
        constraint' (Unchecked.defaultof< ^``Vec<'a>`` >)
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
