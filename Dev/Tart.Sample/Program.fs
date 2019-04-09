namespace wraikny.Tart.Sample

open System
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

        let rec loop () =
            messenger.TryViewModel |> function
            | Some viewModel ->
                printfn "%s" viewModel
            | None ->
                printfn "fail to get viewModel"
                ()

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
                loop()
            else
                messenger.Stop()
                ()
                    

        messenger.StartAsync() |> ignore
        loop()

        0
