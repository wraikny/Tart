namespace wraikny.Tart.Helper.Algorithm

open wraikny.Tart.Helper.Math
open wraikny.Tart.Helper.Graph

open System.Collections.Generic

open FSharpPlus

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module SpanningTree =
    let kruskal nodeCount (edges : Edge<_, _> list) =
        
        let uf = UnionFold(nodeCount)

        let sortedEdges =
            edges
            |> sortBy Edge.weight

        let resultEdges = new List<Edge<_, _>>()

        for edge in sortedEdges do
            if not <| uf.InSameSet(edge) then
                uf.Unite(edge)
                resultEdges.Add(edge)

        seq {
            for edge in resultEdges -> edge
        } |> toList