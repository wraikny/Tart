namespace wraikny.Tart.Helper.Math

[<Struct>]
type ^a Vec2 =
    {
        x : ^a
        y : ^a
    }

    static member inline (~-) (a : ^b Vec2) : ^b Vec2 =
        {
            x = -a.x
            y = -a.y
        }

    static member inline (+) (a : ^b Vec2, b : ^b Vec2) : ^b Vec2 =
        {
            x = a.x + b.x
            y = a.y + b.y
        }

    static member inline (-) (a : ^b Vec2, b : ^b Vec2) : ^b Vec2 =
        {
            x = a.x - b.x
            y = a.y - b.y
        }

    static member inline (*) (a : ^b Vec2, b : ^b Vec2) : ^b Vec2 =
        {
            x = a.x * b.x
            y = a.y * b.y
        }

    static member inline (/) (a : ^b Vec2, b : ^b Vec2) : ^b Vec2 =
        {
            x = a.x / b.x
            y = a.y / b.y
        }


[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Vec2 =
    [<CompiledName "Init">]
    let inline init(x, y) : ^a Vec2 =
        { x = x; y = y }

    [<CompiledName "Zero">]
    let inline zero() : ^a Vec2 =
        let zero = LanguagePrimitives.GenericZero
        init(zero, zero)

    [<CompiledName "FromScalar">]
    let inline fromScalar a = init(a, a)

    [<CompiledName "X">]
    let inline x v = v.x

    [<CompiledName "Y">]
    let inline y v = v.y

    [<CompiledName "XY">]
    let inline xy v = v.x, v.y

    [<CompiledName "Map">]
    let inline map (f : ^a -> ^b) (v : ^a Vec2) : ^b Vec2 =
        {x = f v.x; y = f v.y }

    [<CompiledName "Dot">]
    let inline dot (a : ^a Vec2) (b : ^a Vec2) : ^a =
        a.x * b.x + a.y * b.y

    [<CompiledName "Angle">]
    let inline angle(v : ^a Vec2) : ^a =
        atan2 v.y v.x