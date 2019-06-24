namespace wraikny.Tart.Helper.Collections

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Seq =
    [<CompiledName "FilterMap">]
    let filterMap f : #seq<'a> -> seq<'b> =
        Seq.map f
        >> Seq.filter Option.isSome
        >> Seq.map Option.get

    [<CompiledName "TryAssoc">]
    let tryAssoc key : #seq<'a * 'b> -> 'b option =
        Seq.tryFind (fst >> (=) key)
        >> Option.map snd