module wraikny.Tart.Tests.Main

open Expecto

[<EntryPoint>]
let main argv =
    let x = Tests.runTestsInAssembly defaultConfig argv

    printfn "Enter..."
    System.Console.ReadLine() |> ignore

    x