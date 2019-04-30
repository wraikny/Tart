namespace wraikny.Tart.Helper.Geometry

open wraikny.Tart.Helper.Math

type ^a Rect =
    {
        position : ^a Vec2
        size : ^a Vec2
    }


[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Rect =
    [<CompiledName "Init">]
    let inline init(p, s) : _ Rect =
        { position = p; size = s }

    [<CompiledName "Zero">]
    let inline zero() : ^a Rect =
        let zero = Vec2.zero()
        init(zero, zero)

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

    // TODO : unresolved type parameter issue
    [<CompiledName "DiagonalPosition">]
    let inline diagonalPosition (r : ^a Rect) : ^a Vec2 =
        Vec2.init(
            r.position.x + r.size.x
            , r.position.y + r.size.y
        )

    let inline private get_LU_RD (r : ^a Rect) : (^a Vec2 * ^a Vec2) =
        r.position, diagonalPosition r


    [<CompiledName "IsCollided">]
    let inline isCollided (a : ^a Rect, b : ^a Rect) : bool =
        let aLU, aRD = get_LU_RD a
        let bLU, bRD = get_LU_RD b

        let isCollided =
            not (aRD.x < bLU.x || bRD.x < aLU.x)
            && not (aRD.y < bLU.y || bRD.y < aLU.y)

        isCollided

