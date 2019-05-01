module wraikny.Tart.Sample.Advanced.Dungeon

open wraikny.Tart.Advanced.Dungeon

let generate() =
    let builder : DungeonBuilder = {
        seed = 0
        roomCount = 100
        roomGeneratedRange = (5.0f, 5.0f)
        minRoomSize = (6, 6)
        maxRoomSize = (20, 20)
        roomMoveRate = 0.000001f
        roomMeanThreshold = 1.25f
        restoreEdgeRate = 0.1f
        corridorWidth = 3
    }

    let dungeonModel =
        builder
        |> DungeonBuilder.generate

    //printfn "Large"
    //for s in (dungeonModel.largeRooms |> Map.toSeq) do
    //    printfn "%A" s

    //printfn "Small"
    //for s in (dungeonModel.smallRooms |> Map.toSeq) do
    //    printfn "%A" s

    //printfn "Corridor"
    //for s in (dungeonModel.corridors |> Map.toSeq) do
    //    printfn "%A" s

    //printfn "Edges"
    //for e in (dungeonModel.largeRoomEdges) do
    //    printfn "%A" e

    //printfn "Cells"
    //for s in (dungeonModel.cells |> Map.toSeq) do
    //    printfn "%A" s
    printfn "----------------------------------------"
    printfn "----------------------------------------"
    // printfn "%A" dungeonModel