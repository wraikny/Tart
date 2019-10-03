module wraikny.Tart.Core.Libraries.File

open System.IO
open System.Text
open System.Runtime.Serialization.Formatters.Binary


let private binFormatter = BinaryFormatter()



open System.IO.IsolatedStorage


type 'a StorageIO = StorageIO of (unit -> 'a)
with
    member inline internal x.F = x |> function StorageIO f -> f

module IsolatedStorage =
    let getStorage() = IsolatedStorageFile.GetUserStoreForApplication()

    let rec private createDirectories(path, storage : IsolatedStorageFile) =
        let dirName = Path.GetDirectoryName(path)
        if dirName <> null then
            if not <| storage.DirectoryExists(dirName) then
                createDirectories(dirName, storage)
                storage.CreateDirectory(dirName)

    let readText(path : string) =
        StorageIO <| fun() ->
            try
                let storage = getStorage()
                use stream = storage.OpenFile(path, FileMode.Open, FileAccess.Read)
                use reader = new StreamReader(stream)

                Ok <| reader.ReadToEnd()
            with e -> Error e
        
    //let readTextAsync(path : string) =
    //    StorageIO <| fun() ->
    //        try
    //            let storage = getStorage()
    //            use stream = storage.OpenFile(path, FileMode.Open, FileAccess.Read)
    //            use reader = new StreamReader(stream)

    //            async {
    //                try
    //                    let! x =
    //                        reader.ReadToEndAsync()
    //                        |> Async.AwaitTask
    //                    return (Ok x)
    //                with e -> return(Error e)
    //            }
    //        with e -> async { return Error e }

    let writeText(path : string) (text : string) =
        StorageIO <| fun() ->
            try
                let storage = getStorage()
                createDirectories(path, storage)
                
                if storage.FileExists(path) then
                    storage.DeleteFile(path)
    
                use stream = storage.CreateFile(path)
                use writer = new StreamWriter(stream)
    
                writer.Write(text)
                |> Ok
    
            with e -> Error e

    //let writeTextAsync(path) (text : string) =
    //    StorageIO <| fun() ->
    //        try
    //            let storage = getStorage()
    //            createDirectories(path, storage)
                
    //            if storage.FileExists(path) then
    //                storage.DeleteFile(path)
    
    //            use stream = storage.CreateFile(path)
    //            use writer = new StreamWriter(stream)

    //            async {
    //                try
    //                    do! writer.WriteAsync(text) |> Async.AwaitTask
    //                    return Ok()
    //                with e -> return Error e
    //            }
    //        with e -> async {return Error e}