namespace wraikny.Tart.Helper.Math

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

    static member inline (+) (a, b) =
        {
            x = a.x + b.x
            y = a.y + b.y
            z = a.z + b.z
            w = a.w + b.w
        }

    static member inline (-) (a, b) =
        {
            x = a.x - b.x
            y = a.y - b.y
            z = a.z - b.z
            w = a.w - b.w
        }

    static member inline (*) (a, b) =
        {
            x = a.x * b.x
            y = a.y * b.y
            z = a.z * b.z
            w = a.w * b.w
        }
  
    static member inline (*) (a, b) =
        {
            x = a.x * b
            y = a.y * b
            z = a.z * b
            w = a.w * b
        }

    static member inline (*) (a, b) =
        {
            x = a * b.x
            y = a * b.y
            z = a * b.z
            w = a * b.w
        }


    static member inline (/) (a, b) =
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

    [<CompiledName "X">]
    let inline x v = v.x

    [<CompiledName "Y">]
    let inline y v = v.y

    [<CompiledName "Z">]
    let inline z v = v.z

    [<CompiledName "XY">]
    let inline xy (v : ^a Vec4) = v.x, v.y

    [<CompiledName "YZ">]
    let inline yz (v : ^a Vec4) = v.y, v.z

    [<CompiledName "ZW">]
    let inline zw (v : ^a Vec4) = v.z, v.w

    [<CompiledName "WX">]
    let inline wx (v : ^a Vec4) = v.w, v.x

    [<CompiledName "XYZ">]
    let inline xyz (v : ^a Vec4) = v.x, v.y, v.z

    [<CompiledName "YZW">]
    let inline yzw (v : ^a Vec4) = v.y, v.z, v.w

    [<CompiledName "ZWX">]
    let inline zwx (v : ^a Vec4) = v.z, v.w, v.x

    [<CompiledName "WXY">]
    let inline wxy (v : ^a Vec4) = v.w, v.x, v.y

    [<CompiledName "XYZW">]
    let inline xyzw (v : ^a Vec4) = v.x, v.y, v.z, v.w

    [<CompiledName "Map">]
    let inline map (f : ^a -> ^b) (v : ^a Vec4) : ^b Vec4 =
        {x = f v.x; y = f v.y; z = f v.z; w = f v.w }

    [<CompiledName "Dot">]
    let inline dot (a : ^a Vec4) (b : ^a Vec4) : ^a =
        a.x * b.x + a.y * b.y + a.z * b.z + a.w * b.w