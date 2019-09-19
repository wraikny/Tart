module wraikny.Tart.Core.Libraries.Random
open wraikny.Tart.Core
open FSharpPlus

type 'a Generator = private | Generator of (System.Random -> 'a)
with
    member inline internal g.F = g |> function | Generator f -> f

    static member Return(x : 'a) = Generator(fun _ -> x)
    static member (>>=) (x : 'a Generator, f : 'a -> 'b Generator) : 'b Generator =
        Generator(fun rand ->
            ((x.F rand) |> f).F rand
        )

    static member Map(x : 'a Generator, f : 'a -> 'b) =
        Generator(fun rand -> x.F rand |> f)


let inline bind(f : 'a -> 'b Generator) (x : 'a Generator) : 'b Generator =
    Generator.(>>=)(x, f)


let bool : bool Generator =
    Generator(fun rand ->
        rand.Next() % 2 = 0
    )

    
let int (minValue : int) (maxValue : int) : int Generator =
    Generator(fun rand ->
        rand.Next(minValue, maxValue)
    )


let float (minValue : float) (maxValue : float) : float Generator =
    Generator(fun rand ->
        minValue + rand.NextDouble() * (maxValue - minValue)
    )

let double01 : float Generator =
    Generator(fun rand -> rand.NextDouble())


let list (length : int) (generator : 'a Generator) : 'a list Generator =
    Generator(fun rand ->
        [ for _ in 1..length -> generator.F rand ]
    )

let inline until f (generator : 'a Generator) : 'a Generator =
    monad {
        let rec loop xs = monad {
            if f xs then
                return xs
            else
                let! nx = generator
                return! loop nx
        }
        let! x = generator
        return! loop x
    }


let inline distinctList (length : int) (generator : 'a Generator) : 'a list Generator =
    list length generator
    |> until (fun xs -> xs |> Seq.distinct |> Seq.length = length)


let inline map (f : 'a -> 'b) (x : 'a Generator) : 'b Generator =
    Generator.Map(x, f)


let inline map2 (f : 'a -> 'b -> 'c) (g1 : 'a Generator) (g2 : 'b Generator) : 'c Generator =
    monad {
        let! x1 = g1
        let! x2 = g2
        return (f x1 x2)
    }


let inline pair (g1 : 'a Generator) (g2 : 'b Generator) : ('a * 'b) Generator =
    map2 (fun a b -> a, b) g1 g2