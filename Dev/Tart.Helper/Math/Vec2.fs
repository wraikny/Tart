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

    static member inline ( .* ) (a : ^b, b : ^b Vec2) : ^b Vec2 =
        {
            x = a * b.x
            y = a * b.y
        }

    static member inline ( *. ) (a : ^b Vec2, b : ^b) : ^b Vec2 =
        {
            x = a.x * b
            y = a.y * b
        }

    static member inline (/) (a : ^b Vec2, b : ^b Vec2) : ^b Vec2 =
        {
            x = a.x / b.x
            y = a.y / b.y
        }

    static member inline (./) (a : ^b, b : ^b Vec2) : ^b Vec2 =
        {
            x = a / b.x
            y = a / b.y
        }

    static member inline (/.) (a : ^b Vec2, b : ^b) : ^b Vec2 =
        {
            x = a.x / b
            y = a.y / b
        }


[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Vec2 =
    [<CompiledName "Init">]
    let inline init(x, y) : ^a Vec2 =
        { x = x; y = y }

    let inline internal init1 x = init(x, x)

    [<CompiledName "X">]
    let inline x v = v.x

    [<CompiledName "Y">]
    let inline y v = v.y

    [<CompiledName "Axes">]
    let inline axes() : (^a Vec2 -> ^a) list = [x; y]

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