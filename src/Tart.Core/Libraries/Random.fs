namespace wraikny.Tart.Core.Libraries
open wraikny.Tart.Core
open FSharpPlus

module Random =
    type 'a Generator = internal | Generator of (System.Random -> 'a)
    with
        member inline internal g.F = g |> function | Generator(f) -> f

        static member Return(x : 'a) = Generator(fun _ -> x)
        static member (>>=) (x : 'a Generator, f : 'a -> 'b Generator) =
            Generator(fun rand ->
                ((x.F rand) |> f).F rand
            )

        static member FromEnv(generator : 'a Generator, env : IEnvironment) : 'a =
            generator.F env.Random


    // let inline private getFunc (g : _ Generator) = g.F
    
    [<CompiledName "Bind">]
    let inline bind(f : 'a -> 'b Generator) (x : 'a Generator) : 'b Generator =
        x >>= f

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
            [ for _ in 1..length -> generator.F rand ]
        )



    [<CompiledName "Map">]
    let inline map (f : 'a -> 'b) (x : 'a Generator) : 'b Generator =
        map f x


    [<CompiledName "Map2">]
    let inline map2 (f : 'a -> 'b -> 'c) (g1 : 'a Generator) (g2 : 'b Generator) : 'c Generator =
        monad {
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
        |> Cmd.initMsg
