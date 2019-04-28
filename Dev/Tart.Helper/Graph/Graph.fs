namespace wraikny.Tart.Helper.Graph

type Node<'V> = {
    label : int
    value : 'V
}

type Edge< 'V, 'W
    when 'W : comparison
    > =
    {
        node1 : Node<'V>
        node2 : Node<'V>
        weight : 'W
    }


[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Edge =
    [<CompiledName "Node1">]
    let node1 e = e.node1

    [<CompiledName "Node2">]
    let node2 e = e.node2

    [<CompiledName "Weight">]
    let weight e = e.weight