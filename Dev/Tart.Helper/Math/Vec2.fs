namespace wraikny.Tart.Helper.Math

open wraikny.Tart.Helper.Extension
open FSharpPlus

[<Struct>]
type ^a Vec2 =
    {
        x : ^a
        y : ^a
    }

    static member inline X v = v.x
    static member inline Y v = v.y

    static member inline Init x y = { x = x; y = y }

    /// Foldable
    static member inline ToSeq v =
        seq {
            yield v.x
            yield v.y
        }

    /// Applicative
    static member inline Return (k : ^t) = Vec2< ^t >.Init k k
    static member inline (<*>) (f, v : _ Vec2) = { x = f.x v.x; y = f.y v.y }

    static member inline private Map2 f (a : ^t Vec2) (b : ^t Vec2) : ^t Vec2 = map2 f a b

    // --------------------------------------------------------------------------------

    static member inline Zero (_ : 'T Vec2, _) = Vec2<'T>.Return zero

    static member inline One (_ : 'T Vec2, _) = Vec2<'T>.Return one

    static member inline Abs (v : _ Vec2) = abs <!> v

    static member inline (~-) (v : _ Vec2)= (~-) <!> v

    static member inline (+) (a, b) = Vec2<_>.Map2 (+) a b
    static member inline (-) (a, b) = Vec2<_>.Map2 (-) a b
    static member inline (*) (a, b) = Vec2<_>.Map2 (*) a b
    static member inline (/) (a, b) = Vec2<_>.Map2 (/) a b


[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Vec2 =
    [<CompiledName "Init">]
    let inline init x y = Vec2<_>.Init x y

    [<CompiledName "X">]
    let inline x v = v.x

    [<CompiledName "Y">]
    let inline y v = v.y

    [<CompiledName "Axes">]
    let inline axes() = [x; y]

    [<CompiledName "Angle">]
    let inline angle(v : ^a Vec2) : ^a =
        atan2 v.y v.x