namespace wraikny.Tart.Helper.Graph


[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Kruskal =
    [<CompiledName "Calculate">]
    let calculate num lines =
        let lines = []
        let uf = new UnionFold(num)


        []