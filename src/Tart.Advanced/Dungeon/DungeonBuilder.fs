namespace wraikny.Tart.Advanced.Dungeon

open System
open wraikny.Tart.Math

open FSharpPlus

type DungeonBuilder = {
    /// 乱数生成に用いるシード値
    seed : int

    /// 生成する部屋の数
    roomCount : int

    /// 部屋を生成する楕円の範囲
    roomGeneratedRange : float32 * float32

    /// 生成する部屋のマスの最小数
    minRoomSize : int * int
    /// 生成する部屋のマスの最大数
    maxRoomSize : int * int

    /// x << 1.0f
    roomMoveRate : float32

    /// メインの部屋を決定する部屋するしきい値に使う平均に対する割合。
    /// 1.25f程度で良い結果が得られる。
    roomMeanThreshold : float32

    /// 木から復元するエッジの割合(0.0f ~ 1.0f)。
    /// 0.08f ~ 0.15f で良い結果が得られる。
    restoreEdgeRate : float32

    /// 生成する廊下の幅
    corridorWidth : int
}


type private WithRandom = {
    parameter : DungeonBuilder
    random : Random
}

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module private WithRandom =
    let init (parameter : DungeonBuilder) : WithRandom =
        {
            parameter = parameter
            random = Random(parameter.seed)
        }

    let getRandomValue (builder) =
        builder.random.NextDouble() |> float32

    let roomRects (builder : WithRandom) =
        let getRandomPointInCircle () : int Vec2 =
            let w, h = builder.parameter.roomGeneratedRange

            let t = 2.0f * pi * getRandomValue(builder)
            let u = getRandomValue(builder) * getRandomValue(builder)
            let r = if u > 1.0f then 2.0f - u else u

            (w * r * cos(t), h * r * sin(t))
            |> uncurry Vec2.init
            |>> int

        [for _ in 1..builder.parameter.roomCount ->
            let pos = getRandomPointInCircle()

            let size =
                let min =
                    builder.parameter.minRoomSize
                    |> uncurry Vec2.init
                    |>> float32

                let max =
                    builder.parameter.maxRoomSize
                    |> uncurry Vec2.init
                    |>> float32

                let rand = builder |> getRandomValue

                (min + (max - min) .* rand)
                |>> int

            Rect.init (pos - size ./ 2) size
        ]
        


    open wraikny.Tart.Math.Algorithm
        
        
    let getLargeRoomEdges (largeRooms : (int * int Rect2) list) (withRandom : WithRandom) : Edge<unit, float32> list =
        let largeRoomsCount = largeRooms |> length
        
        let largeRoomEdges =
            largeRooms
            |>> fun (id, rect) ->
                let pos = rect.position |>> float32
                let size = rect.size |>> float32
                Node.init id (pos + size ./ 2.0f)
            
            |> Delaunay2.getNodesList
        
        let largeRoomsSpanningTree =
            largeRoomEdges
            |> SpanningTree.kruskal largeRoomsCount
        
        let largeRoomEdgesResult =
            seq {
                yield! largeRoomsSpanningTree
        
                let exclusiondSpanningTree =
                    largeRoomEdges
                    |> filter(fun e ->
                        largeRoomsSpanningTree
                        |> exists(fun e0 -> Edge.equal e e0)
                        |> not
                    )
        
                let restoreCount =
                    let count = length exclusiondSpanningTree |> float32
                    count * withRandom.parameter.restoreEdgeRate |> int
        
                yield!
                    exclusiondSpanningTree
                    |> sortBy(fun _ -> withRandom.random.NextDouble() )
                    |> take restoreCount
            } |> toList
        
        largeRoomEdgesResult
        |>> map ignore


[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module DungeonBuilder =
    let private distributeRooms (rooms : int Rect2 list) (rate : float32) =
        let threshold =
            let len = rooms |> length |> float32
            let sum : float32 Vec2 =
                rooms
                |>> ( Rect.size >> (map float32) )
                |> sum
            sum .* (rate / len)

        rooms
        |> List.partition(fun rect ->
            let size = rect.size |>> float32
            size.x >= threshold.x && size.y > threshold.y
        )

    open System.Collections.Generic
    // open System.Linq


    let private moveRooms (rooms : int Rect2 list) movingRate : int Rect2 list =
        let roomsList = List<MovingRoom>()

        for r in rooms do
            let movingRoom = MovingRoom(r |>> map float32, movingRate, roomsList)
            roomsList.Add(movingRoom)


        let mutable count = 1.0f
        while roomsList.Exists(fun r -> r.IsMoving) do
            count <- count + 1.0f

            for r in roomsList do r.Update(count)


        [ for r in roomsList -> r.RectI ]


    let private generateCorridors (width) (rect1 : int Rect2, rect2 : int Rect2) : int Rect2 list =
        
        let center1, center2 = Rect.centerPosition rect1, Rect.centerPosition rect2
        let middle = (center1 + center2) ./ 2

        let lurd1 = rect1 |> Rect.get_LU_RD
        let lurd2 = rect2 |> Rect.get_LU_RD

        let isCollidedX = Rect.isCollidedAxis Vector.x lurd1 lurd2
        let isCollidedY = Rect.isCollidedAxis Vector.y lurd1 lurd2

        let manhattanDist = (center1 - center2) |>> abs

        let sizeDict =
            Vec2.init
                ( Vec2.init manhattanDist.x width )
                ( Vec2.init width manhattanDist.y )

        let createCorridorAt size pos =
            Rect.init (pos - size ./ 2) size


        if isCollidedX && isCollidedY then
            []
        elif isCollidedX then
            [ createCorridorAt sizeDict.y middle ]
        elif isCollidedY then
            [ createCorridorAt sizeDict.x middle ]
        else
            seq {
                let f = createCorridorAt
                for center in [center1; center2 ] do
                    yield f (sizeDict.x + Vec2.init width 0) ({ middle with y = center.y})
                    yield f (sizeDict.y + Vec2.init 0 width) ({ middle with x = center.x})
            }
            |> toList


    let private spacesToHashMap =
        List.map(fun (r : Space) ->
            r.id |> SpaceID.value, r
        )
        >> HashMap.ofList


    [<CompiledName "Generate">]
    let generate (builder : DungeonBuilder) : DungeonModel =
        let withRandom = builder |> WithRandom.init

        let roomRects = withRandom |> WithRandom.roomRects

        let movedRooms = moveRooms roomRects builder.roomMoveRate

        let largeRoomRects, smallRoomRects =
            distributeRooms movedRooms builder.roomMeanThreshold


        let largeRoomRectsIndexed = largeRoomRects |> List.indexed


        let largeRooms, smallRooms =
            let inline toRoom kind = map <| fun (i, r) -> Space.init (kind i) r

            largeRoomRectsIndexed |> toRoom SpaceID.Large
            , smallRoomRects |> List.indexed |> toRoom SpaceID.Small


        let largeRoomsHashMap = spacesToHashMap largeRooms


        let largeRoomsEdges =
            withRandom
            |> WithRandom.getLargeRoomEdges largeRoomRectsIndexed


        let corridorRects =
            seq {
                for e in largeRoomsEdges do
                    let room1 =
                        largeRoomsHashMap
                        |> HashMap.find e.node1.label

                    let room2 =
                        largeRoomsHashMap
                        |> HashMap.find e.node2.label

                    yield!
                        generateCorridors builder.corridorWidth (room1.rect, room2.rect)
            }
            |> toList

        let corridors =
            corridorRects
            |> mapi (fun i r ->
                Space.init (SpaceID.Corridor i) r)


        let collidedSmallRooms =
            smallRooms
             |> filter(fun room ->
                 corridorRects
                 |> exists(fun cr ->
                     room.rect
                     |> Rect2.isCollided cr
                 )
             )

        let cellsMap =
            let inline getCells (spaces : Space list) =
                Seq.collect Space.cells spaces

            seq {
                let cellsDict = new Dictionary<int Vec2, SpaceID>()

                for (cdn, id) in (getCells largeRooms) do
                    cellsDict.[cdn] <- id

                for (cdn, id) in (getCells collidedSmallRooms) do
                    if not <| cellsDict.ContainsKey(cdn) then
                        cellsDict.[cdn] <- id

                for (cdn, id) in (getCells corridors) do
                    if not <| cellsDict.ContainsKey(cdn) then
                        cellsDict.[cdn] <- id

                for item in cellsDict ->
                    (item.Key, item.Value)
            }
            |> HashMap.ofSeq


        {
            largeRooms = largeRoomsHashMap
            smallRooms = collidedSmallRooms |> spacesToHashMap
            corridors = corridors |> spacesToHashMap

            largeRoomEdges = largeRoomsEdges

            cells = cellsMap
        }