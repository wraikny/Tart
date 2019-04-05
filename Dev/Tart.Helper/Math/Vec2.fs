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


module Vec2 =
    let inline init(x, y) : ^a Vec2 =
        { x = x; y = y }

    let inline zero() : ^a Vec2 =
        let zero = LanguagePrimitives.GenericZero
        init(zero, zero)

    let inline map (f : ^a -> ^b) (v : ^a Vec2) : ^b Vec2 =
        {x = f v.x; y = f v.y }

    let inline dot (a : ^a Vec2) (b : ^a Vec2) : ^a =
        a.x * b.x + a.y * b.y

    let inline squaredLength(v : ^a Vec2) : ^a =
        dot v v

    let inline length (v : ^a Vec2) : ^a =
        sqrt (squaredLength v)

    let inline normalize(v : ^a Vec2) : ^a Vec2 =
        v * ((length v) ** -LanguagePrimitives.GenericOne)