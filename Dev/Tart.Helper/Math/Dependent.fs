module wraikny.Tart.Helper.Math.Dependent

open wraikny.Tart.Helper.Extension
open FSharpPlus

// https://7colou.red/blog/2018/03-08-fsharp-type-level-computation/index.html

type Ty<'Type> = struct end

let inline ty< ^Type > : Ty< ^Type > = Ty()

let inline eval (x: Ty< ^A >) : Ty< ^B > =
    (^A: (static member inline eval: Ty< ^A > -> Ty< ^B >) x)

[<Struct>]
type Peano = private | Peano

[<Struct>]
type Z = Z with
    static member inline PeanoImpl (_ : Z) = Peano

    static member inline toInt(_ : Z) : int  = 0

    static member eval (x: Ty<Z>) = x

    static member inline sub (x, _: Ty<Z>) = eval x


type S< ^n > = S of ^n with
    static member inline PeanoImpl (_ : ^m S) : _
        when ^m : (static member PeanoImpl : ^m -> Peano) = Peano

    member inline s.Child = s |> function S(n) -> n

    static member inline toInt(_ : S<Z>) : int = 1

    static member inline toInt(s : S< ^m >)
        = (^m : (static member toInt : ^m -> int) s.Child) + 1

    static member inline eval (_: Ty<S< ^N >>) : _
        when ^N: (static member eval: Ty< ^N > -> Ty< ^N' >) = ty<S< ^N' >>

    static member inline sub (_: Ty<S< ^X> >, _: Ty<S< ^Y >>) : _
        when ^X: (static member eval: Ty< ^X > -> Ty< ^X' >)
        and ^Y: (static member eval: Ty< ^Y > -> Ty< ^Y' >)
        and ^Y': (static member sub: Ty< ^X' > * Ty< ^Y' > -> Ty< ^Z >) =
        ty< ^Z >

type Sub<'x, 'y> = Sub of 'x * 'y with
    static member inline eval (_: Ty<Sub< ^X, ^Y>>) : _
        when ^X: (static member eval: Ty< ^X > -> Ty< ^X' >)
        and ^Y: (static member eval: Ty< ^Y > -> Ty< ^Y' >) =
        (^Y': (static member sub: _*_ -> _) ty< ^X' >,ty< ^Y' >)

type ^N GE1 = ^N S
type ^N GE2 = ^N GE1 S
type ^N GE3 = ^N GE2 S
type ^N GE4 = ^N GE3 S

type N0 = Z
type N1 = Z GE1
type N2 = Z GE2
type N3 = Z GE3
type N4 = Z GE4


module Peano =
    let n0 = Z
    let n1 = S Z
    let n2 = S n1
    let n3 = S n2
    let n4 = S n3

    let x = n0
    let y = n1
    let z = n2
    let w = n3


[<Struct>]
type Vector<'a, ^n
    when ^n : (static member PeanoImpl : ^n -> Peano)
    and  ^n : (static member toInt : ^n -> int)
    and  ^n : (static member eval : Ty< ^n > -> Ty< ^n >)
    > = private Vec of 'a [] * ^n with

    member inline private x.n = x |> function | Vec(_, n) -> n
    member inline private x.array = x |> function | Vec(a, _) -> a

    static member inline ToSeq (v : Vector<'t, 'N>) : seq<'t> =
        Array.toSeq v.array

    member inline v.MapArray(f : 'a [] -> 't []) : Vector<'t, 'n> =
        v |> function Vec(arr, n) -> Vec(f arr, n)

    static member inline Map(v : Vector<'t, 'N>, f : 't -> 'u) : Vector<'u, 'N> =
        v.MapArray(map f)


    static member inline (<*>) (f : Vector<'t -> 'u, 'N>, v : Vector<'t, 'N>) : Vector<'u, 'N> =
        let a =
            Array.zip f.array v.array
            |>> fun (f, x) -> f x
        Vec(a, v.n)

    member inline v.Item (i : ^i) : 'a =
        eval ty<Sub<Sub< ^n, ^i >, N1>> |> ignore
        let index = (^i : (static member toInt : ^i -> int) i)
        v.array.[index]
    

type VectorImpl = VectorImpl with
    static member inline Init () =
        Vec( [||], Peano.n0 )

    static member inline Init (x) =
        Vec( [|x|], Peano.n1 )

    static member inline Init (x, y) =
        Vec( [|x; y|], Peano.n2 )

    static member inline Init (x, y, z) =
        Vec( [|x; y; z|], Peano.n3 )

    static member inline Init (x, y, z, w) =
        Vec( [|x; y; z; w|], Peano.n4 )

    static member inline Unsafe(arr, n) = Vec(arr, n)

    static member inline Return (k : 't, _ : N1) = VectorImpl.Init (k)
    static member inline Return (k : 't, _ : N2) = VectorImpl.Init (k, k)
    static member inline Return (k : 't, _ : N3) = VectorImpl.Init (k, k, k)
    static member inline Return (k : 't, _ : N4) = VectorImpl.Init (k, k, k, k)

type 'a Vector0 = Vector<'a, N0>
type 'a Vector1 = Vector<'a, N1>
type 'a Vector2 = Vector<'a, N2>
type 'a Vector3 = Vector<'a, N3>
type 'a Vector4 = Vector<'a, N4>


module Vector2 =
    let inline init x y = VectorImpl.Init(x, y)

    let inline rev (v : 'a Vector2) =
        let x, y = v.Item Peano.x, v.Item Peano.y
        init y x

module Vector3 =
    let inline init x y z = VectorImpl.Init(x, y, z)

module Vector4 =
    let inline init x y z w = VectorImpl.Init(x, y, z, w)


module Vector =
    let inline x (v : Vector<_, 'N GE1>) =
        v.Item Peano.x

    let inline y (v : Vector<_, 'N GE2>) =
        v.Item Peano.y

    let inline z (v : Vector<_, 'N GE3>) =
        v.Item Peano.z

    let inline w (v : Vector<_, 'N GE4>) =
        v.Item Peano.w

    // -------------------------------------
    let inline private kk f (v : Vector<_, _>) =
        let a = f v
        Vector2.init a a

    let inline xx v = kk x v
    let inline yy v = kk y v
    let inline zz v = kk z v
    let inline ww v = kk w v


    type _Access = struct
        static member inline kk v k1 k2=
            Vector2.init (k1 v) (k2 v)
    end

    let inline wz v = _Access.kk v w z
    let inline wy v = _Access.kk v w y
    let inline wx v = _Access.kk v w z

    let inline zy v = _Access.kk v z y
    let inline zx v = _Access.kk v z x

    let inline yx v = _Access.kk v y x

    let inline xy v = (yx v).MapArray(rev)
    let inline xz v = (zx v).MapArray(rev)
    let inline xw v = (wx v).MapArray(rev)

    let inline yz v = (zy v).MapArray(rev)
    let inline yw v = (wy v).MapArray(rev)

    let inline zw v = (wz v).MapArray(rev)
    
    open FSharpPlus.Math.Applicative
    let inline dot (a : Vector<'a, 'n>) (b : Vector<'a, 'n>) =
        a .*. b
        |> fold ( + ) zero

    let inline squaredLength (v : Vector<'a, 'n>) : 'a =
        dot v v

    let inline length (v : Vector<'a, 'n>) : 'a =
        FSharp.Core.Operators.sqrt (squaredLength v)

    let inline normalize (v : Vector<'a, 'n>) : Vector<'a, 'n> =
        v ./ length v
        

module private Test =
    let v0 : int Vector0 = VectorImpl.Init()

    let v1 = VectorImpl.Init 1
    let v1_x = v1 |> Vector.x
    let v1_0 = v1.Item Peano.x
    // let v1_1 = Vector1<_>.Item(v1, Peano.y)

    let v2 = Vector2.init 1 2
    let v2_x = v2.Item Peano.x
    let v2_y = v2.Item Peano.y

    let v2f = float32 <!> v2