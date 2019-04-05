namespace wraikny.Tart.Helper.Math

type ^a Vec3 =
    {
        x : ^a
        y : ^a
        z : ^a
    }

    static member inline (~-) a =
        {
            x = -a.x
            y = -a.y
            z = -a.z
        }

    static member inline (+) (a, b) =
        {
            x = a.x + b.x
            y = a.y + b.y
            z = a.z + b.z
        }

    static member inline (-) (a, b) =
        {
            x = a.x - b.x
            y = a.y - b.y
            z = a.z - b.z
        }

    static member inline (*) (a, b) =
        {
            x = a.x * b.x
            y = a.y * b.y
            z = a.z * b.z
        }
  
    static member inline (*) (a, b) =
        {
            x = a.x * b
            y = a.y * b
            z = a.z * b
        }

    static member inline (*) (a, b) =
        {
            x = a * b.x
            y = a * b.y
            z = a * b.z
        }


    static member inline (/) (a, b) =
        {
            x = a.x / b
            y = a.y / b
            z = a.z / b
        }


module Vec3 =
    let inline init(x, y, z) : ^a Vec3 =
        { x = x; y = y; z = z }

    let inline zero() : ^a Vec3 =
        let zero = LanguagePrimitives.GenericZero
        init(zero, zero, zero)

    let inline map (f : ^a -> ^b) (v : ^a Vec3) : ^b Vec3 =
        {x = f v.x; y = f v.y; z = f v.z }

    let inline dot (a : ^a Vec3) (b : ^a Vec3) : ^b =
        a.x * b.x + a.y * b.y + a.z * b.z

    let inline squaredLength(v : ^a Vec3) : ^b =
        dot v v

    let inline length (v : ^a Vec3) : ^b =
        sqrt (squaredLength v)

    let inline normalize(v : ^a Vec3) : ^b Vec3=
        let len : ^c = (length v)
        let one : ^d = LanguagePrimitives.GenericOne
        v * (len ** -one)