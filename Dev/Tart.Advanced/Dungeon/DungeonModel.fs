namespace wraikny.Tart.Advanced.Dungeon

open wraikny.Tart.Helper.Math
open wraikny.Tart.Helper.Geometry
open wraikny.Tart.Helper.Graph

type SpaceID =
    | Large of int
    | Small of int
    | Corridor of int


[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module SpaceID =
    [<CompiledName "ID">]
    let id id = id |> function
        | Large id
        | Small id
        | Corridor id -> id


type Space = {
    id : SpaceID
    rect : int Rect
}


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
    largeRooms : Map<int, Space>
    smallRooms : Map<int, Space>
    corridors : Map<int, Space>

    /// 大部屋のIDをノードに、距離を重みにもつノードのリスト
    largeRoomEdges : Edge<unit, float32> list

    cells : Map<int Vec2, SpaceID>
}

module DungeonModel =
    [<CompiledName "TryFindSpace">]
    let tryFindSpace id dungeon =
        let target, id = id |> function
            | Large id -> dungeon.largeRooms, id
            | Small id -> dungeon.smallRooms, id
            | Corridor id -> dungeon.corridors, id

        target
        |> Map.tryFind id

        
    [<CompiledName "GetSpaceAt">]
    let getSpaceAt coordinate dungeon =
        dungeon.cells
        |> Map.tryFind coordinate
        |> Option.bind(fun id ->
            tryFindSpace id dungeon
        )