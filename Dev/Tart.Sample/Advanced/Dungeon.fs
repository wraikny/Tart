module wraikny.Tart.Sample.Advanced.Dungeon

open wraikny.Tart.Advanced.Dungeon
open wraikny.Tart.Helper.Geometry
open wraikny.Tart.Helper.Graph
open wraikny.Tart.Helper.Math

let generate() =
    let builder : DungeonBuilder = {
        seed = 0
        roomCount = 200
        roomGeneratedRange = (100.0f, 100.0f)
        minRoomSize = (6, 6)
        maxRoomSize = (20, 20)
        roomMoveRate = 0.2f
        roomMeanThreshold = 1.3f
        restoreEdgeRate = 0.1f
        corridorWidth = 3
    }

    let dungeonModel =
        builder
        |> DungeonBuilder.generate


    // printfn "%A" dungeonModel

    printfn "----------------------------------------"
    printfn "----------------------------------------"

    printfn "Large: %d" <| (Map.toSeq >> Seq.length) dungeonModel.largeRooms
    printfn "Small: %d" <| (Map.toSeq >> Seq.length) dungeonModel.smallRooms
    printfn "Corridor: %d" <| (Map.toSeq >> Seq.length) dungeonModel.corridors
    printfn "Edges: %d" <| (Seq.length) dungeonModel.largeRoomEdges
    printfn "Cells: %d" <| (Map.toSeq >> Seq.length) dungeonModel.cells

    
    asd.Engine.Initialize("Dungeon", 1600, 900, new asd.EngineOption()) |> ignore

    let scene = new asd.Scene()

    let corridors = new asd.Layer2D()
    let smalls = new asd.Layer2D()
    let larges = new asd.Layer2D()
    let edges = new asd.Layer2D()

    let camera() =
        let ws = asd.Engine.WindowSize
        new asd.CameraObject2D(
            Src = new asd.RectI(-ws / 2, ws)
            , Dst = new asd.RectI(new asd.Vector2DI(0, 0), ws)
        )

    corridors.AddObject(camera())
    smalls.AddObject(camera())
    larges.AddObject(camera())
    edges.AddObject(camera())

    scene.AddLayer(corridors)
    scene.AddLayer(smalls)
    scene.AddLayer(larges)
    scene.AddLayer(edges)

    let n = 5.0f

    let create (r : int Rect) =
        let r = r |> Rect.map1 (float32 >> (*) n)
        let rect =
            new asd.RectangleShape(
                DrawingArea =
                    new asd.RectF(r.position.x, r.position.y, r.size.x, r.size.y)
            )
        new asd.GeometryObject2D(Shape = rect)

    for (id, c) in (dungeonModel.corridors |> Map.toSeq) do
        let c = create c.rect
        c.Color <- new asd.Color(255uy, 255uy, 0uy, 255uy)
        corridors.AddObject(c)

    for (id, c) in (dungeonModel.smallRooms |> Map.toSeq) do
        let c = create c.rect
        c.Color <- new asd.Color(0uy, 0uy, 255uy, 200uy)
        corridors.AddObject(c)

    for (id, c) in (dungeonModel.largeRooms |> Map.toSeq) do
        let c = create c.rect
        c.Color <- new asd.Color(255uy, 0uy, 0uy, 200uy)
        corridors.AddObject(c)

    for e in dungeonModel.largeRoomEdges do
        let n1, n2 = e.node1.label, e.node2.label
        let s1, s2 =
            dungeonModel.largeRooms |> Map.find n1
            , dungeonModel.largeRooms |> Map.find n2
        let p1, p2 =
            s1.rect |> Rect.map1 (float32 >> (*) n) |> Rect.centerPosition
            , s2.rect |> Rect.map1 (float32 >> (*) n) |> Rect.centerPosition

        let line =
            new asd.LineShape(
                StartingPosition = new asd.Vector2DF(p1.x, p1.y)
                , EndingPosition = new asd.Vector2DF(p2.x, p2.y)
                , Thickness = 3.0f
            )

        let g = new asd.GeometryObject2D(Shape = line)
        edges.AddObject(g)

        ()

    asd.Engine.ChangeScene scene

    while asd.Engine.DoEvents() do
        asd.Engine.Update()

    asd.Engine.Terminate()