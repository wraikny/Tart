namespace wraikny.Tart.Helper.Geometry

open wraikny.Tart.Helper.Math

[<Struct>]
type Rect< ^a, ^Vec
        when (VectorBuiltin or ^Vec) :
            (static member VectorImpl : ^Vec -> VectorClass< ^a, ^Vec >)
    > =
    {
        position : ^Vec
        size : ^Vec
    }


[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Rect =
    [<CompiledName "Init">]
    let inline init position size =
        { position = position; size = size }

    [<CompiledName "Zero">]
    let inline zero() =
        let zero = VectorClass.zero()
        init zero zero

    [<CompiledName "Position">]
    let inline position r = r.position

    [<CompiledName "Size">]
    let inline size r = r.size

    [<CompiledName "Left">]
    let inline left r = r.position.x

    [<CompiledName "Right">]
    let inline right r = r.position.x + r.size.x

    [<CompiledName "Up">]
    let inline up r = r.position.y

    [<CompiledName "Down">]
    let inline down r = r.position.y + r.size.y

    [<CompiledName "Map">]
    let inline map f rect =
        {
            position = rect.position |> f
            size = rect.size |> f
        }

    [<CompiledName "Map1">]
    let inline map1 f = (Vec2.map f) |> map

    [<CompiledName "DiagonalPosition">]
    let inline diagonalPosition (r : Rect< ^a, ^Vec >) : ^Vec =
        r.position + r.size

    [<CompiledName "CenterPosition">]
    let inline centerPosition (r : Rect< ^a, ^Vec >) : ^Vec =
        let one = LanguagePrimitives.GenericOne
        let two = one + one
        r.position + r.size / (VectorClass.fromScalar two)

    [<CompiledName "Get_LU_RD">]
    let inline get_LU_RD (r : Rect< ^a, ^Vec >) : (^Vec * ^Vec) =
        r.position, diagonalPosition r


    [<CompiledName "IsCollidedAxis">]
    let inline isCollidedAxis(axis : ^Vec -> ^a) (aLU, aRD) (bLU, bRD) : bool =
        not (axis aRD < axis bLU || axis bRD < axis aLU)

    [<CompiledName "IsInside">]
    let inline isInside p r =
        let lu, rd = get_LU_RD r
        [Vec2.x; Vec2.y]
        |> List.map(fun axis ->
            (axis lu) <= (axis p)
            && (axis p) <= (axis rd)
        )
        |> List.fold (&&) true

    [<CompiledName "IsCollided">]
    let inline isCollided (a : Rect< ^a, ^Vec >) (b : Rect< ^a, ^Vec >) : bool =
        let aLURD = get_LU_RD a
        let bLURD = get_LU_RD b

        let isCollided =
            VectorClass.axes()
            |> List.map(fun axis -> isCollidedAxis axis aLURD bLURD)
            |> List.fold (&&) true

        isCollided

