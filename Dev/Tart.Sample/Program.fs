namespace wraikny.Tart.Sample

open System
open System.Threading
open wraikny.Tart.Helper.Monad


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
            printfn "View: %s" view

            printfn "q: Quit, a: Add, s: Sub, c:Clear"
            printf "Input Messege:"
            let continueFlag =
                Console.ReadLine() |> function
                | "q" -> false
                | "a" | "s" as s ->
                    printf "Input Number:"
                    readLineInt() |> function
                    | None -> ()
                    | Some num ->
                        num
                        |> if s = "a" then Counter.Msg.Add else Counter.Msg.Sub
                        |> messenger.PushMsg

                    true

                | "c" ->
                    messenger.PushMsg(Counter.Msg.Clear)
                    true
                | _ ->
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
