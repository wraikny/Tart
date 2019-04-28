namespace wraikny.Tart.Helper.Math

type ^a Vec2 =
    {
        x : ^a
        y : ^a
    }

    static member inline (~-) a =
        {
            x = -a.x
            y = -a.y
        }

    static member inline (+) (a, b) =
        {
            x = a.x + b.x
            y = a.y + b.y
        }

    static member inline (-) (a, b) =
        {
            x = a.x - b.x
            y = a.y - b.y
        }

    static member inline (*) (a, b) =
        {
            x = a.x * b.x
            y = a.y * b.y
        }
  
    static member inline (*) (a, b) =
        {
            x = a.x * b
            y = a.y * b
        }

    static member inline (*) (a, b) =
        {
            x = a * b.x
            y = a * b.y
        }


    static member inline (/) (a, b) =
        {
            x = a.x / b
            y = a.y / b
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

    [<CompiledName "X">]
    let inline x v = v.x

    [<CompiledName "Y">]
    let inline y v = v.y

    [<CompiledName "Map">]
    let inline map (f : ^a -> ^b) (v : ^a Vec2) : ^b Vec2 =
        {x = f v.x; y = f v.y }

    [<CompiledName "Dot">]
    let inline dot (a : ^a Vec2) (b : ^a Vec2) : ^b =
        a.x * b.x + a.y * b.y

    [<CompiledName "SquaredLength">]
    let inline squaredLength(v : ^a Vec2) : ^b =
        dot v v

    [<CompiledName "Length">]
    let inline length (v : ^a Vec2) : ^b =
        sqrt (squaredLength v)

    [<CompiledName "Normalize">]
    let inline normalize(v : ^a Vec2) : ^b Vec2=
        let len : ^c = (length v)
        let one : ^d = LanguagePrimitives.GenericOne
        v * (len ** -one)

    [<CompiledName "Angle">]
    let inline angle(v : ^a Vec2) : ^b =
        atan2 v.y v.x