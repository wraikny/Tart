namespace wraikny.Tart.Helper.Math

#nowarn "0064"

// https://7colou.red/blog/2018/02-14-fsharp-typeclasses/index.html

open FSharpPlus
open FSharpPlus.Math.Applicative

[<Struct>]
type Vector< 'a, '``Vec<'a>``> = Vector of 'a * '``Vec<'a>``


[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module VectorBuiltin =
    let inline impl (v : ^``Vec<'a>``) =
        Vector ( (^``Vec<'a>`` : (static member X : ^``Vec<'a>`` -> ^a) v), v)


type VectorBuiltin = VectorBuiltin with
    static member inline VectorImpl(v : 'a Vec2) =
        VectorBuiltin.impl v
    
    static member inline VectorImpl(v : 'a Vec3) =
        VectorBuiltin.impl v

    static member inline VectorImpl(v : 'a Vec4) =
        VectorBuiltin.impl v


[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Vector =
    [<CompiledName "GetImpl">]
    let inline private getImpl' (_ : ^Builtin) (_ : Vector< ^a, ^Va > ) =
        ((^Builtin or ^Va) : (static member VectorImpl : ^Va -> Vector< ^a, ^Va >)
            Unchecked.defaultof<_>
        )
         
    let inline getImpl v = getImpl' VectorBuiltin v

    let inline constraint' v = getImpl v |> ignore

    [<CompiledName "Axes">]
    let inline axes() : (^``Vec<'a>`` -> ^a) list =
        constraint' (Unchecked.defaultof<Vector< ^a, ^``Vec<'a>`` >>)

        (^``Vec<'a>`` : (static member Axes : unit -> (^``Vec<'a>`` -> ^a) list) ())
        

    [<CompiledName "Dot">]
    let inline dot (a : ^``Vec<'a>``) (b : ^``Vec<'a>``) : ^a =
        constraint' (Unchecked.defaultof<Vector< ^a, ^``Vec<'a>`` >>)

        a * b |> fold ( + ) zero

    [<CompiledName "SquaredLength">]
    let inline squaredLength (v : ^``Vec<'a>``) : ^a =
        constraint' (Unchecked.defaultof<Vector< ^a, ^``Vec<'a>`` >>)

        dot v v

    [<CompiledName "Length">]
    let inline length (v : ^``Vec<'a>``) : ^a =
        constraint' (Unchecked.defaultof<Vector< ^a, ^``Vec<'a>`` >>)

        FSharp.Core.Operators.sqrt (squaredLength v)

    [<CompiledName "Normalize">]
    let inline normalize (v : ^``Vec<'a>``) : ^``Vec<'a>`` =
        constraint' (Unchecked.defaultof<Vector< ^a, ^``Vec<'a>`` >>)

        v ./ length v

    // -----------------------------------------

    [<CompiledName "X">]
    let inline x (a : ^``Vec<'a>``) : ^a =
        constraint' (Unchecked.defaultof<Vector< ^a, ^``Vec<'a>`` >>)
        (^``Vec<'a>`` : (static member X : ^``Vec<'a>`` -> ^a) a)

    [<CompiledName "Y">]
    let inline y (a : ^``Vec<'a>``) : ^a =
        constraint' (Unchecked.defaultof<Vector< ^a, ^``Vec<'a>`` >>)
        (^``Vec<'a>`` : (static member Y : ^``Vec<'a>`` -> ^a) a)

    [<CompiledName "Z">]
    let inline z (a : ^``Vec<'a>``) : ^a =
        constraint' (Unchecked.defaultof<Vector< ^a, ^``Vec<'a>`` >>)
        (^``Vec<'a>`` : (static member Z : ^``Vec<'a>`` -> ^a) a)

    [<CompiledName "W">]
    let inline w (a : ^``Vec<'a>``) : ^a =
        constraint' (Unchecked.defaultof<Vector< ^a, ^``Vec<'a>`` >>)
        (^``Vec<'a>`` : (static member W : ^``Vec<'a>`` -> ^a) a)

    // -----------------------------------------

    let inline private kk (v : ^``Vec<'a>``) k1 k2 = Vec2.init (k1 v) (k2 v)

    let inline xx (v : ^``Vec<'a>``) = kk v x x
    let inline xy (v : ^``Vec<'a>``) = kk v x y
    let inline xz (v : ^``Vec<'a>``) = kk v x z
    let inline xw (v : ^``Vec<'a>``) = kk v x w

    let inline yx (v : ^``Vec<'a>``) = kk v y x
    let inline yy (v : ^``Vec<'a>``) = kk v y y
    let inline yz (v : ^``Vec<'a>``) = kk v y z
    let inline yw (v : ^``Vec<'a>``) = kk v y w

    let inline zx (v : ^``Vec<'a>``) = kk v z x
    let inline zy (v : ^``Vec<'a>``) = kk v z y
    let inline zz (v : ^``Vec<'a>``) = kk v z z
    let inline zw (v : ^``Vec<'a>``) = kk v z w

    let inline wx (v : ^``Vec<'a>``) = kk v w x
    let inline wy (v : ^``Vec<'a>``) = kk v w y
    let inline wz (v : ^``Vec<'a>``) = kk v w z
    let inline ww (v : ^``Vec<'a>``) = kk v w w

    // -----------------------------------------

    [<CompiledName "XYZ">]
    let inline xyz (v : ^``Vec<'a>``) =
        Vec3.init (x v) (y v) (z v)