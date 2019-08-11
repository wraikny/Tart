﻿namespace wraikny.Tart.Helper.Math

open FSharpPlus
open FSharpPlus.Math.Applicative

[<Struct>]
type ^a Vec3 =
    {
        x : ^a
        y : ^a
        z : ^a
    }

    static member inline Init x y z = { x = x; y = y; z = z }
    
    static member inline Axes() : ('t Vec3 -> 't) list =
        [
            fun v -> v.x
            fun v -> v.y
            fun v -> v.z
        ]

    /// Foldable
    static member inline ToSeq (v : 't Vec3) =
        Vec3<'t>.Axes() |>> (|>) v |> toSeq
    
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


[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Vec3 =
    [<CompiledName "Init">]
    let inline init x y z : ^a Vec3 = Vec3<_>.Init x y z

    [<CompiledName "X">]
    let inline x v = v.x

    [<CompiledName "Y">]
    let inline y v = v.y

    [<CompiledName "Z">]
    let inline z v = v.z