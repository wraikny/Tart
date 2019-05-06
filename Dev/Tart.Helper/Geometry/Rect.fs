namespace wraikny.Tart.Helper.Geometry

open wraikny.Tart.Helper.Math

[<Struct>]
type ^a Rect =
    {
        position : ^a Vec2
        size : ^a Vec2
    }


[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Rect =
    [<CompiledName "Init">]
    let inline init position size : _ Rect =
        { position = position; size = size }

    [<CompiledName "Zero">]
    let inline zero() : ^a Rect =
        let zero = Vec2.zero()
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
    let inline diagonalPosition (r : ^a Rect) : ^a Vec2 =
        Vec2.init(
            r.position.x + r.size.x
            , r.position.y + r.size.y
        )

    [<CompiledName "CenterPosition">]
    let inline centerPosition (r : ^a Rect) : ^a Vec2 =
        let one = LanguagePrimitives.GenericOne
        let two = one + one
        Vec2.init(
            r.position.x + r.size.x / two
            , r.position.y + r.size.y / two
        )

    let inline get_LU_RD (r : ^a Rect) : (^a Vec2 * ^a Vec2) =
        r.position, diagonalPosition r


    [<CompiledName "IsCollidedAxis">]
    let inline isCollidedAxis(axis : ^a Vec2 -> ^a) (aLU, aRD) (bLU, bRD) : bool =
        not (axis aRD < axis bLU || axis bRD < axis aLU)


    [<CompiledName "IsCollided">]
    let inline isCollided (a : ^a Rect) (b : ^a Rect) : bool =
        let aLURD = get_LU_RD a
        let bLURD = get_LU_RD b

        let isCollided =
            (isCollidedAxis Vec2.x aLURD bLURD)
            && (isCollidedAxis Vec2.y aLURD bLURD)

        isCollided

