namespace wraikny.Tart.Helper.Graph

open wraikny.Tart.Helper.Math
open wraikny.Tart.Helper.Geometry


[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Delaunay2 =
    /// 全体を包含する正三角形を求める
    [<CompiledName "GetHugeTriangle">]
    let getHugeTriangle (range : float32 Rect) : Triangle<float32, Vec2<float32>> =
        let leftUp = range.position
        let rightDown = range.position + range.size

        // 与えられた矩形を包含する円を求める  
        let center = (rightDown - leftUp) / 2.0f
        let radius = Vec2.length (leftUp - center)

        // その円に外接する正三角形を求める  
        let sqrt3 = sqrt(3.0f)
        let bottom = center.y - radius

        let x1 = center.x - sqrt3 * radius
        let p1 = Vec2.init(x1, bottom)

        let x2 = center.x + sqrt3 * radius
        let p2 = Vec2.init(x2, bottom)

        let y3 = center.y + 2.0f * radius
        let p3 = Vec2.init(center.x, y3)

        Triangle.init(p1, p2, p3)

    open System.Collections.Generic
    open System.Linq

    [<CompiledName "GetTrianglesList">]
    let getTrianglesList (range) (points : float32 Vec2 list) : Triangle<float32, Vec2<float32>> list =
        let hugeTriangle = getHugeTriangle range

        let trianglesSet = new HashSet<Triangle2Float32>()
        let addTriangleToSet tri =
            trianglesSet.Add( new Triangle2Float32(tri) )

        addTriangleToSet(hugeTriangle) |> ignore

        // 点を逐次添加し、反復的に三角分割を行う  
        for p in points do
            
            /// 追加候補の三角形を保持する一時ハッシュ
            /// Key : 三角形
            /// Value : 重複しているか
            let tmpTriangleSet = new Dictionary<Triangle2Float32, bool>()
            let addToTmpSet tri =
                let tri = new Triangle2Float32(tri)
                let isDuplicated = not <| tmpTriangleSet.ContainsKey(tri)
                if isDuplicated then
                    tmpTriangleSet.Add(tri, isDuplicated)
                else
                    tmpTriangleSet.[tri] <- isDuplicated


            // 現在の三角形リストから要素を一つずつ取り出して、  
            // 与えられた点が各々の三角形の外接円の中に含まれるかどうか判定  
            for t in (new HashSet<_>(trianglesSet)) do
                // 外接円
                let c = Triangle.circumscribedCircle t.Triangle

                let sqDistance = Vec2.squaredLength (c.center - p)

                if(sqDistance < c.radius * c.radius) then
                    let tri = t.Triangle
                    addToTmpSet( Triangle.init(p, tri.p1, tri.p2) )
                    addToTmpSet( Triangle.init(p, tri.p2, tri.p3) )
                    addToTmpSet( Triangle.init(p, tri.p3, tri.p1) )
                    
                    trianglesSet.Remove(t) |> ignore

            
            // 一時ハッシュのうち、重複のないものを三角形リストに追加
            for pair in tmpTriangleSet do
                if(pair.Value) then
                    trianglesSet.Add(pair.Key) |> ignore


        let trianglesHavingCommonPointOfHuge =
            trianglesSet.Where(fun x -> Triangle.hasCommonPoint x.Triangle hugeTriangle)

        
        for t in trianglesHavingCommonPointOfHuge do
            trianglesSet.Remove(t) |> ignore
        

        seq {
            for t in trianglesSet -> t.Triangle
        }
        |> Seq.toList