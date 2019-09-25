namespace wraikny.Tart.Math.Algorithm

open wraikny.Tart.Math

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

        [ for edge in resultEdges -> edge ]
