namespace wraikny.Tart.Advanced.Dungeon

open wraikny.Tart.Helper.Math
open wraikny.Tart.Helper.Geometry
open wraikny.Tart.Helper.Graph
open wraikny.Tart.Helper.Collections

[<Struct>]
type SpaceID =
    | Large of large:int
    | Small of small:int
    | Corridor of corridor:int


[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module SpaceID =
    [<CompiledName "ID">]
    let id id = id |> function
        | Large id
        | Small id
        | Corridor id -> id


[<Struct>]
type Space = {
    id : SpaceID
    rect : int Rect2
}


[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Space =
    let internal init id rect = {
        id = id
        rect = rect
    }

    [<CompiledName "ID">]
    let id s = s.id

    [<CompiledName "Rect">]
    let rect s = s.rect


type DungeonModel = {
    /// IDと大部屋の対応
    largeRooms : HashMap<int, Space>
    /// IDと小さい部屋の対応
    smallRooms : HashMap<int, Space>
    /// IDと廊下の対応
    corridors : HashMap<int, Space>

    /// 大部屋のIDをノードのラベルに、距離を重みにもつノードのリスト
    largeRoomEdges : Edge<unit, float32> list

    /// マスの座標と空間IDの対応
    cells : HashMap<int Vec2, SpaceID>
}


[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module DungeonModel =
    [<CompiledName "TryFindSpace">]
    let tryFindSpace id dungeon =
        let target, id = id |> function
            | Large id -> dungeon.largeRooms, id
            | Small id -> dungeon.smallRooms, id
            | Corridor id -> dungeon.corridors, id

        target
        |> HashMap.tryFind id

        
    [<CompiledName "GetSpaceAt">]
    let getSpaceAt coordinate dungeon =
        dungeon.cells
        |> HashMap.tryFind coordinate
        |> Option.bind(fun id ->
            tryFindSpace id dungeon
        )


    [<CompiledName "CellToCoordinate">]
    let cellToCoordinate (cellSize : float32 Vec2) (cell : int Vec2) : float32 Vec2 =
        let cellf = cell |> Vec2.map float32
        cellf * cellSize


    [<CompiledName "CoordinateToCell">]
    let coordinateToCell (cellSize : float32 Vec2) (coordinate : float32 Vec2) : int Vec2 =
        coordinate / cellSize
        |> Vec2.map (floor >> int)