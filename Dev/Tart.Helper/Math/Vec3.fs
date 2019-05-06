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

    static member inline (/) (a : ^b Vec3, b : ^b Vec3) : ^b Vec3 =
        {
            x = a.x / b.x
            y = a.y / b.y
            z = a.z / b.z
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

    [<CompiledName "FromScalar">]
    let inline fromScalar a = init(a, a, a)

    [<CompiledName "X">]
    let inline x v = v.x

    [<CompiledName "Y">]
    let inline y v = v.y

    [<CompiledName "Z">]
    let inline z v = v.z

    [<CompiledName "XY">]
    let inline xy (v : ^a Vec3) = v.x, v.y

    [<CompiledName "YZ">]
    let inline yz (v : ^a Vec3) = v.y, v.z

    [<CompiledName "ZX">]
    let inline zx (v : ^a Vec3) = v.z, v.x

    [<CompiledName "XYZ">]
    let inline xyz (v : ^a Vec3) = v.x, v.y, v.z

    [<CompiledName "Map">]
    let inline map (f : ^a -> ^b) (v : ^a Vec3) : ^b Vec3 =
        {x = f v.x; y = f v.y; z = f v.z }

    [<CompiledName "Dot">]
    let inline dot (a : ^a Vec3) (b : ^a Vec3) : ^a =
        a.x * b.x + a.y * b.y + a.z * b.z