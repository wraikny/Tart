module wraikny.Tart.Tests

open Expecto

[<EntryPoint>]
let main argv =
    try
        let x = Tests.runTestsInAssembly defaultConfig argv

        ()
    with e ->
        printfn "%A" e

    printfn "Enter..."
    System.Console.ReadLine() |> ignore

    0