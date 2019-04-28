namespace wraikny.Tart.Helper.Graph

open wraikny.Tart.Helper.Math

open System.Collections.Generic

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module SpanningTree =
    [<CompiledName "Kruskal">]
    let inline kruskal nodeCount (edges : Edge<_, _> list) =
        
        let uf = new UnionFold(nodeCount)

        let sortedEdges =
            edges
            |> List.sortBy Edge.weight

        let resultEdges = new List<Edge<_, _>>()

        for edge in sortedEdges do
            if not <| uf.InSameSet(edge) then
                uf.Unite(edge)
                resultEdges.Add(edge)

        seq {
            for edge in resultEdges -> edge
        } |> Seq.toList