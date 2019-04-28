namespace wraikny.Tart.Helper.Math

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

    [<CompiledName "DiagonalPosition">]
    let inline diagonalPosition (r) =
        [ Vec2.x; Vec2.y ]
        |>
        Vec2.init(
            r.position.x + r.size.x
            , r.position.y + r.size.y
        )

    //[<CompiledName "IsCollided">]
    //let inline isCollided (a : ^a Rect) (b : ^a Rect) : bool =
    //    let aRU : ^a Vec2 = a.position
    //    let aLD : ^a Vec2 = aRU + a.size
    //    let bRU : ^a Vec2 = b.position
    //    let bLD : ^a Vec2 = bRU + b.size

    //    false