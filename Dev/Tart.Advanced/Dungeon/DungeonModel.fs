namespace wraikny.Tart.Advanced.Dungeon

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
    minRoomSize : int Vec2
    /// 生成する部屋のマスの最大数
    maxRoomSize : int Vec2

    /// メインの部屋を決定する部屋するしきい値に使う平均に対する割合。
    /// 1.25f程度で良い結果が得られる。
    roomMeanThreshold : float32

    /// 木から復元するエッジの割合(0.0f ~ 1.0f)。
    /// 0.08f ~ 0.15f で良い結果が得られる。
    restoreEdgeRate : float32
}


type Room = {
    rect : int Rect
    id : int
}

module Room =
    let internal init rect id = {rect = rect; id = id}


type DungeonModel = {
    largeRooms : Room list
    smallRooms : Room list
    corridors : Room list
}