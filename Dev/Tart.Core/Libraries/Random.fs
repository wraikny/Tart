namespace wraikny.Tart.Core.Libraries
open wraikny.Tart.Core

module Random =
    type Seed = { seed : int }


    let initialSeed (i : int) : Seed =
        {
            seed = i
        }


    let private addSeed (i : int) (seed : Seed) =
        initialSeed(seed.seed + i)


    type 'a Generator = | Generator of (Seed option -> 'a)
    

    let private getFunc (g : 'a Generator) : (Seed option -> 'a) =
        g |> function | Generator(f) -> f


    let step (generator : 'a Generator) (seed : Seed) : 'a * Seed =
        let result = getFunc generator (Some seed)
        let nextSeed = initialSeed <| (new System.Random(seed.seed)).Next()

        result, nextSeed


    let private getRandom(seed : Seed option) : System.Random =
        seed |> function
        | Some({seed=seed}) ->
            new System.Random(seed)
        | None ->
            new System.Random()


    let bool : bool Generator =
        Generator(fun seed ->
            let rand = getRandom seed
            rand.Next() % 2 = 0
        )

    
    let int (minValue : int) (maxValue : int) : int Generator =
        Generator(fun seed ->
            let rand = getRandom seed
            rand.Next(minValue, maxValue)
        )


    let float (minValue : float) (maxValue : float) : float Generator =
        Generator(fun seed ->
            let rand = getRandom seed
            minValue + rand.NextDouble() * (maxValue - minValue)
        )


    let list (length : int) (generator : 'a Generator) : ('a list) Generator =
        Generator(fun seed ->
            seq {
                for index in 0..length ->
                    let seed =
                        seed |> Option.map(fun seed ->
                            initialSeed <| (seed.seed + index)
                        )

                    (getFunc generator seed)
            }
            |> Seq.toList
        )




    let map (f : 'a -> 'b) (generator : 'a Generator) : 'b Generator =
        Generator(fun seed ->
            f (getFunc generator seed)
        )


    let private addSeedOption (i : int) (seed : Seed option) : Seed Option =
        seed |> Option.map (addSeed i)


    let map2 (f : 'a -> 'b -> 'c) (g1 : 'a Generator) (g2 : 'b Generator) : 'c Generator =
        Generator(fun seed ->
            f
                (getFunc g1 seed)
                (getFunc g2 <| addSeedOption 1 seed)
        )


    let map3 (f : 'a -> 'b -> 'c -> 'd)
        (g1 : 'a Generator)
        (g2 : 'b Generator)
        (g3 : 'c Generator)
        : 'd Generator =
        Generator(fun seed ->
            f
                (getFunc g1 seed)
                (getFunc g2 <| addSeedOption 1 seed)
                (getFunc g3 <| addSeedOption 2 seed)
        )


    let map4 (f : 'a -> 'b -> 'c -> 'd -> 'e)
        (g1 : 'a Generator)
        (g2 : 'b Generator)
        (g3 : 'c Generator)
        (g4 : 'd Generator)
        : 'e Generator =
        Generator(fun seed ->
            f
                (getFunc g1 seed)
                (getFunc g2 <| addSeedOption 1 seed)
                (getFunc g3 <| addSeedOption 2 seed)
                (getFunc g4 <| addSeedOption 3 seed)
        )


    let map5 (f : 'a -> 'b -> 'c -> 'd -> 'e -> 'f)
        (g1 : 'a Generator)
        (g2 : 'b Generator)
        (g3 : 'c Generator)
        (g4 : 'd Generator)
        (g5 : 'e Generator)
        : 'f Generator =
        Generator(fun seed ->
            f
                (getFunc g1 seed)
                (getFunc g2 <| addSeedOption 1 seed)
                (getFunc g3 <| addSeedOption 2 seed)
                (getFunc g4 <| addSeedOption 3 seed)
                (getFunc g5 <| addSeedOption 4 seed)
        )


    let pair (g1 : 'a Generator) (g2 : 'b Generator) : ('a * 'b) Generator =
        map2 (fun a b -> a, b) g1 g2


    let andThen(f : 'a -> 'b Generator) (g : 'a Generator) : 'b Generator =
        Generator(fun seed ->
            getFunc ((getFunc g seed) |> f) seed
        )


    let generate (msg : 'a -> 'Msg) (generator : 'a Generator) : 'Msg Cmd =
        (fun pushMsg ->
            pushMsg( (getFunc generator None) |> msg )
        )
        |> Cmd.singleCommand