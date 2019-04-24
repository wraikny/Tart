namespace wraikny.Tart.Helper.Extension

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Seq =
    [<CompiledName "FilterMap">]
    let filterMap f =
        Seq.map f
        >> Seq.filter Option.isSome
        >> Seq.map Option.get

    [<CompiledName "TryAssoc">]
    let tryAssoc key =
        Seq.tryFind (fst >> (=) key)
        >> Option.map snd