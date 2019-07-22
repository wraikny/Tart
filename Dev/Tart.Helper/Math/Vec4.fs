namespace wraikny.Tart.Helper.Math

open wraikny.Tart.Helper.Extension
open FSharpPlus

[<Struct>]
type ^a Vec4 =
    {
        x : ^a
        y : ^a
        z : ^a
        w : ^a
    }

    static member inline X v = v.x
    static member inline Y v = v.y
    static member inline Z v = v.z
    static member inline W v = v.w

    static member inline Init x y z w = { x = x; y = y; z = z; w = w }

    /// Foldable
    static member inline ToSeq v =
        seq {
            yield v.x
            yield v.y
            yield v.z
        }

    /// Applicative
    static member inline Return (k : ^t) = Vec4< ^t >.Init k k k k
    static member inline (<*>) (f, v : _ Vec4) =
        { x = f.x v.x; y = f.y v.y; z = f.z v.z; w = f.w v.w }

    static member inline private Map2 f (a : ^t Vec4) (b : ^t Vec4) : ^t Vec4 = map2 f a b
    static member inline private Map2' f a b = Vec4<_>.Map2 f (Vec4<_>.Return a) b

    // --------------------------------------------------------------------------------

    static member inline Zero (_ : 'T Vec4, _) = Vec4<'T>.Return zero

    static member inline One (_ : 'T Vec4, _) = Vec4<'T>.Return one

    static member inline Abs (v : _ Vec4) = abs <!> v

    static member inline (~-) (v : _ Vec4)= (~-) <!> v

    static member inline (+) (a, b) = Vec4<_>.Map2 (+) a b
    static member inline (-) (a, b) = Vec4<_>.Map2 (-) a b
    static member inline (*) (a, b) = Vec4<_>.Map2 (*) a b
    static member inline (/) (a, b) = Vec4<_>.Map2 (/) a b

    static member inline ( .* ) (a, b) = Vec4<_>.Map2' (*) a b
    static member inline ( *. ) (a, b) = Vec4<_>.Map2' (flip (*)) b a

    static member inline ( ./ ) (a, b) = Vec4<_>.Map2' (/) a b
    static member inline ( /. ) (a, b) = Vec4<_>.Map2' (flip (/)) b a


[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Vec4 =
    [<CompiledName "Init">]
    let inline init x y z w : ^a Vec4 = Vec4<_>.Init x y z w