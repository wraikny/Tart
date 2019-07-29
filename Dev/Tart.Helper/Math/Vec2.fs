namespace wraikny.Tart.Helper.Math

open FSharpPlus
open FSharpPlus.Math.Applicative

[<Struct>]
type ^a Vec2 =
    {
        x : ^a
        y : ^a
    }
    static member inline Init x y = { x = x; y = y }

    static member inline Axes() : ('t Vec2 -> 't) list =
        [
            fun v -> v.x
            fun v -> v.y
        ]

    /// Foldable
    static member inline ToSeq (v : 't Vec2) =
        Vec2<'t>.Axes() |>> (|>) v |> toSeq

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


[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Vec2 =
    [<CompiledName "Init">]
    let inline init x y = Vec2<_>.Init x y

    [<CompiledName "X">]
    let inline x v = v.x

    [<CompiledName "Y">]
    let inline y v = v.y

    [<CompiledName "Angle">]
    let inline angle(v : ^a Vec2) : ^a =
        atan2 v.y v.x