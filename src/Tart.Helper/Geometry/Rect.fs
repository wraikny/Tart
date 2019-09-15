namespace wraikny.Tart.Helper.Geometry

open wraikny.Tart.Helper.Math

open FSharpPlus
open FSharpPlus.Math.Applicative

type 'Vec Rect = {
    position : 'Vec
    size : 'Vec
} with

    static member Init position size = {
        position = position
        size = size
    }

    /// Applicative
    static member inline Return (k : 't) = Rect< 't >.Init k k
    static member (<*>) (f, x : _ Rect) = {
        position = f.position x.position
        size = f.size x.size
    }

    // --------------------------------------------------------------------------------

    static member inline Zero (_ : 'T Rect, _) = Rect<'T>.Return zero

    static member inline One (_ : 'T Rect, _) = Rect<'T>.Return one


type ^a Rect2 = ^a Vec2 Rect
type ^a Rect3 = ^a Vec3 Rect
type ^a Rect4 = ^a Vec4 Rect


[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Rect =
    [<CompiledName "Init">]
    let inline init position size = Rect<_>.Init position size

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

    [<CompiledName "DiagonalPosition">]
    let inline diagonalPosition r : '``Vec<'a>`` =
        r.position + r.size

    [<CompiledName "CenterPosition">]
    let inline centerPosition r : '``Vec<'a>`` =
        let two = one + one
        r.position + r.size / two

    [<CompiledName "Get_LU_RD">]
    let inline get_LU_RD r : ('``Vec<'a>`` * '``Vec<'a>``) =
        r.position, diagonalPosition r


    [<CompiledName "IsCollidedAxis">]
    let inline isCollidedAxis(axis : '``Vec<'a>`` -> 'a) (aLU, aRD) (bLU, bRD) : bool =
        Utils.inCollision (axis aLU, axis aRD) (axis bLU, axis bRD)

    [<CompiledName "IsInside">]
    let inline isInside (p : '``Vec<'a>``) r : bool =
        Vector.constraint' (Unchecked.defaultof<Vector< 'a, '``Vec<'a>`` >>)

        let lu, rd = get_LU_RD r
        
        zip (toSeq lu) (toSeq rd)
        |> zip (toSeq p)
        |>> fun (p', (lu', rd')) ->
            lu' <= p' && p' <= rd'
        |> fold (&&) true

    //open System.Collections.Generic
    //open System.Linq

    // Too Heavy to use !!!!

    //[<CompiledName "IsCollided">]
    //let inline isCollided (a : '``Vec<'a>`` Rect) (b : '``Vec<'a>`` Rect) : bool =
    //    Vector.constraint' (Unchecked.defaultof<Vector< 'a, '``Vec<'a>`` >>)
    //    let aLURD = get_LU_RD a
    //    let bLURD = get_LU_RD b

    //    let axes = Stack<'``Vec<'a>`` -> 'a>( Vector.axes() |> toSeq )

    //    //let isCollided =
            
    //    //    |>> fun axis -> isCollidedAxis axis aLURD bLURD
    //    //    |> fold (&&) true

    //    let rec isCollided() =
    //        if axes.Count > 0 then
    //            if ( isCollidedAxis (axes.Pop()) aLURD bLURD ) then
    //                isCollided()
    //            else
    //                false
    //        else
    //            true
            

    //    isCollided()


module Rect2 =
    [<CompiledName "IsCollided">]
    let inline isCollided (a : ^a Rect2) (b : ^a Rect2) : bool =
        let aLU = a.position
        let aRD = aLU .+. a.size

        let bLU = b.position
        let bRD = bLU .+. b.size

        let inline f (axis : _ -> ^a) =
             not(axis bRD < axis aLU || axis aRD < axis bLU)

        (f Vec2.x) && (f Vec2.y)
