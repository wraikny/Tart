namespace wraikny.Tart.Advanced.Dungeon

open System

open wraikny.Tart.Helper.Math
open wraikny.Tart.Helper.Geometry

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


module private WithRandom =
    let init (parameter : DungeonBuilder) : WithRandom =
        {
            parameter = parameter
            random = new Random(parameter.seed)
        }

    let getRandomValue (builder) =
        builder.random.NextDouble() |> float32

    let roomRects (builder : WithRandom) =
        let getRandomPointInCircle (builder : WithRandom) : int Vec2 =
            let w, h = builder.parameter.roomGeneratedRange

            let t = 2.0f * Angle.pi * getRandomValue(builder)
            let u = getRandomValue(builder) * getRandomValue(builder)
            let r = if u > 1.0f then 2.0f - u else u

            (w * r * cos(t), h * r * sin(t))
            |> Vec2.init
            |> Vec2.map int

        [for _ in 1..builder.parameter.roomCount -> ()]
        |> List.map(fun () ->
            let pos =
                builder
                |> getRandomPointInCircle

            let size =
                let min =
                    builder.parameter.minRoomSize
                    |> Vec2.init
                    |> Vec2.map float32

                let max =
                    builder.parameter.maxRoomSize
                    |> Vec2.init
                    |> Vec2.map float32

                let rand = builder |> getRandomValue

                (min + (max - min) * rand)
                |> Vec2.map int

            Rect.init (pos - size / 2) size
        )


    open wraikny.Tart.Helper.Graph
        
        
    let getLargeRoomEdges (largeRooms : (int * int Rect) list) (withRandom : WithRandom) : Edge<unit, float32> list =
        let largeRoomsCount = largeRooms |> List.length
        
        let largeRoomEdges =
            largeRooms
            |> List.map(fun (id, rect) ->
                let pos = rect.position |> Vec2.map float32
                Node.init (id, pos)
            )
            |> Delaunay2.getNodeList
        
        let largeRoomsSpanningTree =
            largeRoomEdges
            |> SpanningTree.kruskal largeRoomsCount
        
        let largeRoomEdgesResult =
            seq {
                yield! largeRoomsSpanningTree
        
                let exclusiondSpanningTree =
                    largeRoomEdges
                    |> List.filter(fun e ->
                        largeRoomsSpanningTree
                        |> List.exists(fun e0 -> Edge.equal e e0)
                        |> not
                    )
        
                let restoreCount =
                        let count = List.length exclusiondSpanningTree |> float32
                        count * withRandom.parameter.restoreEdgeRate |> int
        
                yield!
                    exclusiondSpanningTree
                    |> List.sortBy(fun _ -> withRandom.random.NextDouble() )
                    |> List.take restoreCount
            } |> List.ofSeq
        
        largeRoomEdgesResult
        |> List.map(
            Edge.mapValues(fun _ -> ())
        )


module DungeonBuilder =
    let private distributeRooms (rooms : int Rect list) (rate : float32) =
        let threshold =
            let len = rooms |> List.length |> float32
            let sum =
                rooms
                |> List.map ( Rect.size >> (Vec2.map float32) )
                |> List.fold (+) (Vec2.zero())
            (sum * rate / len)

        rooms
        |> List.partition(fun rect ->
            let size = rect.size |> Vec2.map float32
            size.x >= threshold.x && size.y > threshold.y
        )

    open System.Collections.Generic
    // open System.Linq


    let private moveRooms (rooms : int Rect list) movingRate : int Rect list =
        let roomsList = new List<MovingRoom>()

        for r in rooms do
            let movingRoom = new MovingRoom(r |> Rect.map1 float32, movingRate, roomsList)
            roomsList.Add(movingRoom)


        while roomsList.Exists(fun r -> r.IsMoving) do
            for r in roomsList do r.Update()

            
        [ for r in roomsList -> r.RectI ]


    let private generateCorridors (width) (rect1 : int Rect, rect2 : int Rect) : int Rect list =
        
        let middle = (rect1.position + rect2.position) / 2

        let lurd1 = rect1 |> Rect.get_LU_RD
        let lurd2 = rect2 |> Rect.get_LU_RD

        let isCollidedX = Rect.isCollidedAxis Vec2.x lurd1 lurd2
        let isCollidedY = Rect.isCollidedAxis Vec2.y lurd1 lurd2

        let manhattanDist =
            let center r = r.position + r.size / 2
            (center rect1 - center rect2)
            |> Vec2.map abs

        let sizeDict =
            Vec2.init(
                Vec2.init(manhattanDist.y + width, width)
                , Vec2.init(width, manhattanDist.y + width)
            )

        let createCorridorAt axis pos =
            let size = axis sizeDict
            Rect.init (pos - size / 2) size


        if isCollidedX && isCollidedY then
            []
        elif isCollidedX then
            [ createCorridorAt Vec2.y middle ]
        elif isCollidedY then
            [ createCorridorAt Vec2.x middle ]
        else
            seq {
                let f = createCorridorAt
                for pos in [rect1.position; rect2.position ] do
                    yield f Vec2.x ({ middle with y = pos.y})
                    yield f Vec2.y ({ middle with x = pos.x})
            }
            |> Seq.toList


    let private spacesToMap =
        List.map(fun (r : Space) ->
            r.id |> SpaceID.id, r
        )
        >> Map.ofList


    open System.Collections.Generic

    [<CompiledName "Generate">]
    let generate (builder : DungeonBuilder) : DungeonModel =
        let withRandom = builder |> WithRandom.init

        let roomRects = withRandom |> WithRandom.roomRects

        let movedRooms = moveRooms roomRects builder.roomMoveRate


        let largeRoomRects, smallRoomRects =
            distributeRooms movedRooms builder.roomMeanThreshold


        let largeRoomRectsIndexed = largeRoomRects |> List.indexed


        let largeRooms, smallRooms =
            let toRoom kind = List.map(fun (i, r) -> Space.init (kind i) r)

            largeRoomRectsIndexed |> toRoom SpaceID.Large
            , smallRoomRects |> List.indexed |> toRoom SpaceID.Small


        let largeRoomsMap = spacesToMap largeRooms


        let largeRoomsEdges =
            withRandom
            |> WithRandom.getLargeRoomEdges largeRoomRectsIndexed


        let corridorRects =
            seq {
                for e in largeRoomsEdges do
                    let room1 =
                        largeRoomsMap
                        |> Map.find e.node1.label

                    let room2 =
                        largeRoomsMap
                        |> Map.find e.node2.label

                    yield!
                        generateCorridors builder.corridorWidth (room1.rect, room2.rect)
            }
            |> Seq.toList


        let corridors =
            corridorRects
            |> List.indexed
            |> List.map(fun (i, r) ->
                Space.init (SpaceID.Corridor i) r
            )


        let collidedSmallRooms =
            smallRooms
            |> List.filter(fun room ->
                corridorRects
                |> List.exists(fun cr ->
                    room.rect
                    |> Rect.isCollided cr
                )
            )

        let cellsMap =
            let getCells (spaces : Space list) =
                spaces
                |> Seq.map(fun space ->
                    let lu, rd = space.rect |> Rect.get_LU_RD
                    seq {
                        for x in (lu.x)..(rd.x) do
                        for y in (lu.y)..(rd.y) do
                            yield ( (x, y), space.id )
                    }
                )
                |> Seq.concat

            seq {
                let cellsDict = new Dictionary<int * int, SpaceID>()

                for (cdn, id) in (getCells largeRooms) do
                    cellsDict.[cdn] <- id

                for (cdn, id) in (getCells collidedSmallRooms) do
                    if not <| cellsDict.ContainsKey(cdn) then
                        cellsDict.[cdn] <- id

                for (cdn, id) in (getCells corridors) do
                    if not <| cellsDict.ContainsKey(cdn) then
                        cellsDict.[cdn] <- id

                for item in cellsDict ->
                    (Vec2.init item.Key, item.Value)
            }
            |> Map.ofSeq


        {
            largeRooms = largeRoomsMap
            smallRooms = collidedSmallRooms |> spacesToMap
            corridors = corridors |> spacesToMap

            largeRoomEdges = largeRoomsEdges

            cells = cellsMap
        }