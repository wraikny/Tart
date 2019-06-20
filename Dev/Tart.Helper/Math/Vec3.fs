namespace wraikny.Tart.Helper.Math

[<Struct>]
type ^a Vec3 =
    {
        x : ^a
        y : ^a
        z : ^a
    }

    static member inline (~-) (a : ^b Vec3) : ^b Vec3 =
        {
            x = -a.x
            y = -a.y
            z = -a.z
        }

    static member inline (+) (a : ^b Vec3, b : ^b Vec3) : ^b Vec3 =
        {
            x = a.x + b.x
            y = a.y + b.y
            z = a.z + b.z
        }

    static member inline (-) (a : ^b Vec3, b : ^b Vec3) : ^b Vec3 =
        {
            x = a.x - b.x
            y = a.y - b.y
            z = a.z - b.z
        }

    static member inline (*) (a : ^b Vec3, b : ^b Vec3) : ^b Vec3 =
        {
            x = a.x * b.x
            y = a.y * b.y
            z = a.z * b.z
        }

    static member inline ( .* ) (a : ^b, b : ^b Vec3) : ^b Vec3 =
        {
            x = a * b.x
            y = a * b.y
            z = a * b.z
        }

    static member inline ( *. ) (a : ^b Vec3, b : ^b) : ^b Vec3 =
        {
            x = a.x * b
            y = a.y * b
            z = a.z * b
        }

    static member inline (/) (a : ^b Vec3, b : ^b Vec3) : ^b Vec3 =
        {
            x = a.x / b.x
            y = a.y / b.y
            z = a.z / b.z
        }

    static member inline (./) (a : ^b, b : ^b Vec3) : ^b Vec3 =
        {
            x = a / b.x
            y = a / b.y
            z = a / b.z
        }

    static member inline (/.) (a : ^b Vec3, b : ^b) : ^b Vec3 =
        {
            x = a.x / b
            y = a.y / b
            z = a.z / b
        }


[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Vec3 =
    [<CompiledName "Init">]
    let inline init(x, y, z) : ^a Vec3 =
        { x = x; y = y; z = z }

    [<CompiledName "Zero">]
    let inline zero() : ^a Vec3 =
        let zero = LanguagePrimitives.GenericZero
        init(zero, zero, zero)

    [<CompiledName "Init1">]
    let inline init1 a = init(a, a, a)

    [<CompiledName "X">]
    let inline x v = v.x

    [<CompiledName "Y">]
    let inline y v = v.y

    [<CompiledName "Z">]
    let inline z v = v.z

    [<CompiledName "Axes">]
    let inline axes() : (^a Vec3 -> ^a) list = [x; y; z]

    [<CompiledName "XYZ">]
    let inline xyz (v : ^a Vec3) = v.x, v.y, v.z

    [<CompiledName "Map">]
    let inline map (f : ^a -> ^b) (v : ^a Vec3) : ^b Vec3 =
        {x = f v.x; y = f v.y; z = f v.z }

    [<CompiledName "Dot">]
    let inline dot (a : ^a Vec3) (b : ^a Vec3) : ^a =
        a.x * b.x + a.y * b.y + a.z * b.z