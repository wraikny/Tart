namespace wraikny.Tart.Helper.Geometry

open wraikny.Tart.Helper.Math

[<Struct>]
type ^Vec Rect =
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

    [<CompiledName "MapVec">]
    let inline mapVec (f : ^a -> ^Ma) : ^Vec Rect -> ^MVec Rect
        when (VectorBuiltin or ^Vec) :
            (static member VectorImpl : ^Vec -> VectorClass< ^a, ^Vec, ^Ma, ^MVec >)
        =
        (VectorClass.map f) |> map 

    [<CompiledName "DiagonalPosition">]
    let inline diagonalPosition r : ^Vec
        when (VectorBuiltin or ^Vec) :
            (static member VectorImpl : ^Vec -> VectorClass< ^a, ^Vec, ^Ma, ^MVec >)
        =
        r.position + r.size

    [<CompiledName "CenterPosition">]
    let inline centerPosition r : ^Vec
        when (VectorBuiltin or ^Vec) :
            (static member VectorImpl : ^Vec -> VectorClass< ^a, ^Vec, ^Ma, ^MVec >)
        =
        let one = LanguagePrimitives.GenericOne
        let two = one + one
        r.position + r.size / (VectorClass.init1 two)

    [<CompiledName "Get_LU_RD">]
    let inline get_LU_RD r : (^Vec * ^Vec)
        when (VectorBuiltin or ^Vec) :
            (static member VectorImpl : ^Vec -> VectorClass< ^a, ^Vec, ^Ma, ^MVec >)
        =
        r.position, diagonalPosition r


    [<CompiledName "IsCollidedAxis">]
    let inline isCollidedAxis(axis : ^Vec -> ^a) (aLU, aRD) (bLU, bRD) : bool
        when (VectorBuiltin or ^Vec) :
            (static member VectorImpl : ^Vec -> VectorClass< ^a, ^Vec, ^Ma, ^MVec >)
        =
        not (axis aRD < axis bLU || axis bRD < axis aLU)

    [<CompiledName "IsInside">]
    let inline isInside p r : bool
        when (VectorBuiltin or ^Vec) :
            (static member VectorImpl : ^Vec -> VectorClass< ^a, ^Vec, ^Ma, ^MVec >)
        =
        let lu, rd = get_LU_RD r
        VectorClass.axes()
        |> List.map(fun axis ->
            (axis lu) <= (axis p)
            && (axis p) <= (axis rd)
        )
        |> List.fold (&&) true

    [<CompiledName "IsCollided">]
    let inline isCollided a b : bool
        when (VectorBuiltin or ^Vec) :
            (static member VectorImpl : ^Vec -> VectorClass< ^a, ^Vec, ^Ma, ^MVec >)
        =
        let aLURD = get_LU_RD a
        let bLURD = get_LU_RD b

        let isCollided =
            VectorClass.axes()
            |> List.map(fun axis -> isCollidedAxis axis aLURD bLURD)
            |> List.fold (&&) true

        isCollided

