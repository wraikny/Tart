namespace wraikny.Tart.Helper.Math

type ^a Rect =
    {
        position : ^a Vec2
        size : ^a Vec2
    }


[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Rect =
    [<CompiledName "Init">]
    let inline init(p, s) : ^a Rect =
        { position = p; size = s }

    [<CompiledName "Zero">]
    let inline zero() : ^a Rect =
        let zero = Vec2.zero()
        init(zero, zero)

    [<CompiledName "Position">]
    let position r = r.position

    [<CompiledName "Size">]
    let size r = r.size
