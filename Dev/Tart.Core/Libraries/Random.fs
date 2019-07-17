namespace wraikny.Tart.Core.Libraries
open wraikny.Tart.Core


module Random =
    type 'a Generator = internal | Generator of (System.Random -> 'a)
    with
        member inline internal g.F = g |> function | Generator(f) -> f

    // let inline private getFunc (g : _ Generator) = g.F
    
    [<CompiledName "Bind">]
    let bind(f : 'a -> 'b Generator) (g : 'a Generator) : 'b Generator =
        Generator(fun rand ->
            ((g.F rand) |> f).F rand
        )

    type RandomBuilder() =
        let mreturn x = Generator(fun _ -> x)
        member __.Bind(x, k) = bind k x
        member __.YieldFrom(x) = x
        member __.ReturnFrom(x) = x
        member __.Yield(x) = mreturn x
        member __.Return(x) = mreturn x
        member __.Delay(f) = f()

        member __.For(inp, f) =
            seq {for a in inp -> f a}

    let random = new RandomBuilder()

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
            [ for _ in 0..length-1 -> generator.F rand ]
        )



    [<CompiledName "Map">]
    let inline map (f : 'a -> 'b) (generator : 'a Generator) : 'b Generator =
        random {
            let! x1 = generator
            return (f x1)
        }


    [<CompiledName "Map2">]
    let inline map2 (f : 'a -> 'b -> 'c) (g1 : 'a Generator) (g2 : 'b Generator) : 'c Generator =
        random {
            let! x1 = g1
            let! x2 = g2
            return (f x1 x2)
        }


    [<CompiledName "Pair">]
    let inline pair (g1 : 'a Generator) (g2 : 'b Generator) : ('a * 'b) Generator =
        map2 (fun a b -> a, b) g1 g2


    [<CompiledName "Generate">]
    let generate (msg : 'a -> 'Msg) (generator : 'a Generator) : Cmd<'Msg, _> =
        (fun (env : IEnvironment) pushMsg ->
            pushMsg( generator.F env.Random |> msg )
        )
        |> Cmd.singleCommand