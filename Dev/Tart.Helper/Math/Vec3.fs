namespace wraikny.Tart.Helper.Math

open wraikny.Tart.Helper.Extension
open FSharpPlus

[<Struct>]
type ^a Vec3 =
    {
        x : ^a
        y : ^a
        z : ^a
    }

    static member inline X v = v.x
    static member inline Y v = v.y
    static member inline Z v = v.z

    static member inline Init x y z = { x = x; y = y; z = z }
    
    /// Foldable
    static member inline ToSeq v =
        seq {
            yield v.x
            yield v.y
            yield v.z
        }
    
    /// Applicative
    static member inline Return (k : ^t) = Vec3< ^t >.Init k k k
    static member inline (<*>) (f, v : _ Vec3) = { x = f.x v.x; y = f.y v.y; z = f.z v.z }
    
    static member inline private Map2 f (a : ^t Vec3) (b : ^t Vec3) : ^t Vec3 = map2 f a b
    
    // --------------------------------------------------------------------------------
    
    static member inline Zero (_ : 'T Vec3, _) = Vec3<'T>.Return zero
    
    static member inline One (_ : 'T Vec3, _) = Vec3<'T>.Return one
    
    static member inline Abs (v : _ Vec3) = abs <!> v
    
    static member inline (~-) (v : _ Vec3)= (~-) <!> v
    
    static member inline (+) (a, b) = Vec3<_>.Map2 (+) a b
    static member inline (-) (a, b) = Vec3<_>.Map2 (-) a b
    static member inline (*) (a, b) = Vec3<_>.Map2 (*) a b
    static member inline (/) (a, b) = Vec3<_>.Map2 (/) a b


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

    [<CompiledName "Axes">]
    let inline axes() =[x; y; z]