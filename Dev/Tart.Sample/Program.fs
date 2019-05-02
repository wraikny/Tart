namespace wraikny.Tart.Sample

open System
open System.Threading




module Program =
    let readLine () = Console.ReadLine()

    let readLineInt() =
        let success, result = readLine () |> Int32.TryParse
        if success then Some result else None


    let counter() =
        let messenger = Counter.messengerBuilder()

        let rec loop view =
            printfn "View: %s" <| view.ToString()

            printfn "q: Quit, a: Add, s: Sub, r:Random, c:Clear"
            printf "Input Messege:"

            let input = readLine()

            let inputNumMsg nextMsg =
                readLineInt () |> function
                | None ->
                    printfn "Input correctly!"

                | Some n ->
                    messenger.PushMsg(nextMsg n)

            input |> function
            | "q" -> ()

            | "a" ->
                printf "Input Number:"
                inputNumMsg Counter.Msg.Add

            | "s" ->
                printf "Input Number:"
                inputNumMsg Counter.Msg.Sub

            | "r" ->
                printf "Input Max of Random Number:"
                inputNumMsg (fun n -> Counter.Msg.Random(0, n))

            | "c" ->
                messenger.PushMsg(Counter.Msg.Clear)

            | _ ->
                printfn "Input correctly!"


            if input <> "q" then
                Thread.Sleep(10)

                messenger.TryViewModel |> function
                | Some view -> loop view
                | None ->
                    printfn "Failed to get viewModel. Display old one."
                    loop view


        messenger.StartAsync() |> ignore

        messenger.TryViewModel |> function
        | Some view -> loop view
        | None -> ()

        messenger.Stop()


        printfn "Enter to exit the program"
        Console.ReadLine() |> ignore


    [<EntryPoint>]
    let main _ = 
        // counter()
        Advanced.Dungeon.generate()

        Console.ReadLine() |> ignore
        0
