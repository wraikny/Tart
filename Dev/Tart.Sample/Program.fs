namespace wraikny.Tart.Sample

open System
open System.Threading


module Program =
    let readLine () = Console.ReadLine()

    let readLineInt() =
        let success, result =
            readLine () |> Int32.TryParse
        if success then Some result else None


    [<EntryPoint>]
    let main argv = 
        let messenger = Counter.messengerBuilder()

        let rec loop view =
            printfn "View: %s" <| view.ToString()

            printfn "q: Quit, a: Add, s: Sub, c:Clear, r:Random"
            printf "Input Messege:"
            let continueFlag =
                Console.ReadLine() |> function
                | "q" -> false
                | "a" | "s" as s ->
                    printf "Input Number:"
                    readLineInt() |> function
                    | None ->
                        printfn "Input correctly!"
                        ()
                    | Some n ->
                        n
                        |> if s = "a" then Counter.Msg.Add else Counter.Msg.Sub
                        |> messenger.PushMsg

                    true

                | "c" ->
                    messenger.PushMsg(Counter.Msg.Clear)
                    true
                | "r" ->
                    printf "Input Max of Random Number:"
                    readLineInt() |> function
                    | None ->
                        printfn "Input correctly!"
                        ()
                    | Some n ->
                        messenger.PushMsg(Counter.Msg.Random(0, n))
                    true
                | _ ->
                    printfn "Input correctly!"
                    true

            if continueFlag then
                Thread.Sleep(10)
                messenger.TryViewModel |> function
                | Some view -> loop view
                | None ->
                    printfn "Failed to get viewModel"
                    loop view
            else
                messenger.Stop()
                ()


        messenger.StartAsync() |> ignore

        messenger.TryViewModel |> function
        | Some view -> loop view
        | None ->
            ()

        printfn "Enter to exit the program"
        Console.ReadLine() |> ignore

        0
