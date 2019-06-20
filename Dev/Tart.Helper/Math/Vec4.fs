namespace wraikny.Tart.Helper.Math

[<Struct>]
type ^a Vec4 =
    {
        x : ^a
        y : ^a
        z : ^a
        w : ^a
    }

    static member inline (~-) a =
        {
            x = -a.x
            y = -a.y
            z = -a.z
            w = -a.w
        }

    static member inline (+) (a : ^b Vec4, b : ^b Vec4) : ^b Vec4 =
        {
            x = a.x + b.x
            y = a.y + b.y
            z = a.z + b.z
            w = a.w + b.w
        }

    static member inline (-) (a : ^b Vec4, b : ^b Vec4) : ^b Vec4 =
        {
            x = a.x - b.x
            y = a.y - b.y
            z = a.z - b.z
            w = a.w - b.w
        }

    static member inline (*) (a : ^b Vec4, b : ^b Vec4) : ^b Vec4 =
        {
            x = a.x * b.x
            y = a.y * b.y
            z = a.z * b.z
            w = a.w * b.w
        }

    static member inline ( .* ) (a : ^b, b : ^b Vec4) : ^b Vec4 =
        {
            x = a * b.x
            y = a * b.y
            z = a * b.z
            w = a * b.w
        }

    static member inline ( *. ) (a : ^b Vec4, b : ^b) : ^b Vec4 =
        {
            x = a.x * b
            y = a.y * b
            z = a.z * b
            w = a.w * b
        }

    static member inline (/) (a : ^b Vec4, b : ^b Vec4) : ^b Vec4 =
        {
            x = a.x / b.x
            y = a.y / b.y
            z = a.z / b.z
            w = a.w / b.w
        }

    static member inline (./) (a : ^b, b : ^b Vec4) : ^b Vec4 =
        {
            x = a / b.x
            y = a / b.y
            z = a / b.z
            w = a / b.w
        }

    static member inline (/.) (a : ^b Vec4, b : ^b) : ^b Vec4 =
        {
            x = a.x / b
            y = a.y / b
            z = a.z / b
            w = a.w / b
        }


[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Vec4 =
    [<CompiledName "Init">]
    let inline init(x, y, z, w) : ^a Vec4 =
        { x = x; y = y; z = z; w = w }

    [<CompiledName "Zero">]
    let inline zero() : ^a Vec4 =
        let zero = LanguagePrimitives.GenericZero
        init(zero, zero, zero, zero)

    [<CompiledName "Init1">]
    let inline init1 a = init(a, a, a, a)

    [<CompiledName "X">]
    let inline x v = v.x

    [<CompiledName "Y">]
    let inline y v = v.y

    [<CompiledName "Z">]
    let inline z v = v.z

    [<CompiledName "W">]
    let inline w v = v.w

    [<CompiledName "Axes">]
    let inline axes() : (^a Vec4 -> ^a) list = [x; y; z; w]

    [<CompiledName "XYZW">]
    let inline xyzw (v : ^a Vec4) = v.x, v.y, v.z, v.w

    [<CompiledName "Map">]
    let inline map (f : ^a -> ^b) (v : ^a Vec4) : ^b Vec4 =
        {x = f v.x; y = f v.y; z = f v.z; w = f v.w }

    [<CompiledName "Dot">]
    let inline dot (a : ^a Vec4) (b : ^a Vec4) : ^a =
        a.x * b.x + a.y * b.y + a.z * b.z + a.w * b.w