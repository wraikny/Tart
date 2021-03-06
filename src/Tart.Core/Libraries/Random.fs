module wraikny.Tart.Core.Libraries.Random

open wraikny.Tart.Helper

type 'a Generator = private | Generator of (System.Random -> 'a)
with
    member inline internal g.F = g |> function | Generator f -> f

    static member Return(x : 'a) = Generator(fun _ -> x)
    static member (>>=) (x : 'a Generator, f : 'a -> 'b Generator) : 'b Generator =
        Generator <|fun rand -> ((x.F rand) |> f).F rand

    static member Map(x : 'a Generator, f : 'a -> 'b) =
        Generator <| fun rand -> x.F rand |> f


let inline bind(f : 'a -> 'b Generator) (x : 'a Generator) : 'b Generator =
    Generator.(>>=)(x, f)


let bool : bool Generator =
    Generator <| fun rand ->
        rand.Next() % 2 = 0

    
let int (minValue : int) (maxValue : int) : int Generator =
    Generator <| fun rand ->
        rand.Next(minValue, maxValue)


let float (minValue : float) (maxValue : float) : float Generator =
    Generator <| fun rand ->
        minValue + rand.NextDouble() * (maxValue - minValue)

let double01 : float Generator =
    Generator <| fun rand -> rand.NextDouble()


let list (length : int) (generator : 'a Generator) : 'a list Generator =
    Generator <|fun rand ->
        [ for _i = 1 to length do yield generator.F rand ]

let inline until f (generator : 'a Generator) : 'a Generator =
    let rec loop xs =
        if f xs then
            pure' xs
        else
            generator >>= fun nx ->
            loop nx

    generator >>= fun x ->
        loop x


let inline distinctList (length : int) (generator : 'a Generator) : 'a list Generator =
    list length generator
    |> until (fun xs -> xs |> Seq.distinct |> Seq.length = length)


let inline map (f : 'a -> 'b) (x : 'a Generator) : 'b Generator =
    Generator.Map(x, f)


let inline map2 (f : 'a -> 'b -> 'c) (g1 : 'a Generator) (g2 : 'b Generator) : 'c Generator =
    g1 >>= fun x1 ->
    g2 >>= fun x2 ->
        f x1 x2 |> pure'


let inline pair (g1 : 'a Generator) (g2 : 'b Generator) : ('a * 'b) Generator =
    map2 (fun a b -> a, b) g1 g2