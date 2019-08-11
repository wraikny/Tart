﻿namespace wraikny.Tart.Helper.Graph

open FSharpPlus

[<Struct>]
type Node<'V> =
    {
        label : int
        value : 'V
    }

    static member Map(node: _ Node, f : 'T -> 'U) =
        { label = node.label; value = f node.value }

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Node =
    [<CompiledName "Init">]
    let init label value = { label = label; value = value }

    [<CompiledName "Equal">]
    let equal (n1 : Node<'a>) (n2 : Node<'a>) =
        n1.label = n2.label


[<Struct>]
type Edge< 'V, 'W
    when 'W : comparison
    > =
    {
        node1 : Node<'V>
        node2 : Node<'V>
        weight : 'W
    }

    static member Map(edge: Edge<_, _>, f : 'T -> 'U) =
        {
            weight = edge.weight
            node1 = edge.node1 |>> f
            node2 = edge.node2 |>> f
        }


[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Edge =
    [<CompiledName "Init">]
    let inline init n1 n2 w = { node1 = n1; node2 = n2; weight = w }

    [<CompiledName "Node1">]
    let inline node1 e = e.node1

    [<CompiledName "Node2">]
    let inline node2 e = e.node2

    [<CompiledName "Weight">]
    let inline weight e = e.weight

    [<CompiledName "Equal">]
    let equal (e1 : Edge<'a, 'b>) (e2 : Edge<'a, 'b>) =
        ( (Node.equal e1.node1 e2.node1) && (Node.equal e1.node2 e2.node2) )
        || ( (Node.equal e1.node1 e2.node2) && (Node.equal e1.node2 e2.node1) )

    [<CompiledName "Values">]
    let inline values edge =
        edge.node1.value, edge.node2.value



open wraikny.Tart.Helper.Math
open wraikny.Tart.Helper.Geometry

type NodeTriangle(node1, node2, node3) =

    let triangle = Triangle.init node1.value node2.value node3.value
    let labelTriangle = Triangle.init node1.label node2.label node3.label

    let edges =
        let createEdge (a : Node<float32 Vec2>) (b : Node<float32 Vec2>) =
            let w = Vector.length(a.value - b.value)
            Edge.init a b w

        let e1 = createEdge node1 node2
        let e2 = createEdge node2 node3
        let e3 = createEdge node3 node1
        (e1, e2, e3)

    member val Node1 : Node< Vec2<float32> > = node1 with get
    member val Node2 : Node< Vec2<float32> > = node2 with get
    member val Node3 : Node< Vec2<float32> > = node3 with get

    /// member val Label = (node1.label, node2.label, node3.label) with get

    member val Triangle = triangle with get
    member val LabelTriangle = labelTriangle with get

    member val Edges = edges with get
    
    override this.GetHashCode() =
        node1.label.GetHashCode()
        ^^^ node2.label.GetHashCode()
        ^^^ node3.label.GetHashCode()
    
    override this.Equals(o) =
        o |> function
        | :? NodeTriangle as other ->
            Triangle.equal this.LabelTriangle other.LabelTriangle
        | _ -> false