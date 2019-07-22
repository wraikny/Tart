namespace wraikny.Tart.Helper.Math

open FSharpPlus
open FSharpPlus.Math.Applicative

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

    static member inline Axes() : ('t Vec4 -> 't) list =
        [ Vec4<'t>.X; Vec4<'t>.Y; Vec4<'t>.Z; Vec4<'t>.W ]

    /// Foldable
    static member inline ToSeq (v : 't Vec4) =
        Vec4<'t>.Axes() |>> (|>) v |> toSeq

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


[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Vec4 =
    [<CompiledName "Init">]
    let inline init x y z w : ^a Vec4 = Vec4<_>.Init x y z w

    [<CompiledName "X">]
    let inline x v = v.x

    [<CompiledName "Y">]
    let inline y v = v.y

    [<CompiledName "Z">]
    let inline z v = v.z

    [<CompiledName "W">]
    let inline w v = v.w