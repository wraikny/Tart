namespace wraikny.Tart.Core.Libraries
open wraikny.Tart.Core


module Random =
    type 'a Generator = | Generator of (System.Random -> 'a)
    

    let private getFunc (g : 'a Generator) : (System.Random -> 'a) =
        g |> function | Generator(f) -> f


    [<CompiledName "Bool">]
    let bool : bool Generator =
        Generator(fun rand ->
            rand.Next() % 2 = 0
        )

    
    [<CompiledName "Int">]
    let int (minValue : int) (maxValue : int) : int Generator =
        Generator(fun rand ->
            rand.Next(minValue, maxValue)
        )


    [<CompiledName "Float">]
    let float (minValue : float) (maxValue : float) : float Generator =
        Generator(fun rand ->
            minValue + rand.NextDouble() * (maxValue - minValue)
        )


    [<CompiledName "List">]
    let list (length : int) (generator : 'a Generator) : ('a list) Generator =
        Generator(fun rand ->
            seq {
                for index in 0..length-1 ->
                    (getFunc generator rand)
            }
            |> Seq.toList
        )



    [<CompiledName "Map">]
    let map (f : 'a -> 'b) (generator : 'a Generator) : 'b Generator =
        Generator(fun rand ->
            f (getFunc generator rand)
        )


    [<CompiledName "Map2">]
    let map2 (f : 'a -> 'b -> 'c) (g1 : 'a Generator) (g2 : 'b Generator) : 'c Generator =
        Generator(fun rand ->
            f
                (getFunc g1 rand)
                (getFunc g2 rand)
        )


    [<CompiledName "Map3">]
    let map3 (f : 'a -> 'b -> 'c -> 'd)
        (g1 : 'a Generator)
        (g2 : 'b Generator)
        (g3 : 'c Generator)
        : 'd Generator =
        Generator(fun rand ->
            f
                (getFunc g1 rand)
                (getFunc g2 rand)
                (getFunc g3 rand)
        )


    [<CompiledName "Map4">]
    let map4 (f : 'a -> 'b -> 'c -> 'd -> 'e)
        (g1 : 'a Generator)
        (g2 : 'b Generator)
        (g3 : 'c Generator)
        (g4 : 'd Generator)
        : 'e Generator =
        Generator(fun rand ->
            f
                (getFunc g1 rand)
                (getFunc g2 rand)
                (getFunc g3 rand)
                (getFunc g4 rand)
        )


    [<CompiledName "Map5">]
    let map5 (f : 'a -> 'b -> 'c -> 'd -> 'e -> 'f)
        (g1 : 'a Generator)
        (g2 : 'b Generator)
        (g3 : 'c Generator)
        (g4 : 'd Generator)
        (g5 : 'e Generator)
        : 'f Generator =
        Generator(fun rand ->
            f
                (getFunc g1 rand)
                (getFunc g2 rand)
                (getFunc g3 rand)
                (getFunc g4 rand)
                (getFunc g5 rand)
        )


    [<CompiledName "Pair">]
    let pair (g1 : 'a Generator) (g2 : 'b Generator) : ('a * 'b) Generator =
        map2 (fun a b -> a, b) g1 g2


    [<CompiledName "AndThen">]
    let andThen(f : 'a -> 'b Generator) (g : 'a Generator) : 'b Generator =
        Generator(fun rand ->
            getFunc ((getFunc g rand) |> f) rand
        )


    [<CompiledName "Generate">]
    let generate (msg : 'a -> 'Msg) (generator : 'a Generator) : Cmd<'Msg, _> =
        (fun (env : IEnvironment) pushMsg ->
            pushMsg( (getFunc generator)(env.Random) |> msg )
        )
        |> Cmd.singleCommand