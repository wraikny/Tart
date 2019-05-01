namespace wraikny.Tart.Advanced.Dungeon

open System.Collections.Generic
open System.Linq

open wraikny.Tart.Helper.Math
open wraikny.Tart.Helper.Geometry


[<Class>]
type internal MovingRoom(rect : float32 Rect, movingRate, rooms) =
    let rooms : IReadOnlyList<MovingRoom> = rooms

    let movingRate : float32 = movingRate

    let size = rect.size
    let mutable position = rect.position
    let mutable lastPosition = position

    let len (o : MovingRoom) = o.Position |> Vec2.squaredLength

    let nearEnough a b =
        let d = 1.0f * movingRate
        Vec2.squaredLength (a - b) < d * d

    member val Position = position with get
    member val RightDown = position + size with get
    member val Center = position + size / 2.0f with get
    member val IsMoving = true with get, set


    member this.Update(count) =
        let targets =
            rooms
                .Where(fun o -> o <> this)
                .Where(fun o -> this.IsCollidedWith o)

        lastPosition <- position

        for other in targets do
            if (len this) > (len other) then
                this.Move(other)
                other.Move(this)
            else
                other.Move(this)
                this.Move(other)

        let d = 0.1f * movingRate * movingRate * count
        this.IsMoving <- Vec2.squaredLength (lastPosition - position) > d * d
    

    member this.RectI
        with get() : int Rect = {
            position = this.Position |> Vec2.map int
            size = size |> Vec2.map int
        }

    member this.RectF
        with get() : float32 Rect = {
            position = this.Position
            size = size
        }


    member private this.IsCollidedWith(other : MovingRoom) =
        Rect.isCollided this.RectF other.RectF


    member this.Move(other : MovingRoom) =
        let getDiff axis =
            if (axis other.Center < axis this.Center) then
                axis other.RightDown - axis this.Position
            else
                axis this.RightDown - axis other.Position

        let dx, dy = getDiff Vec2.x, getDiff Vec2.y

        // let diff = Vec2.init <| if dx < dy then (dx, 0.0f) else (0.0f, dy)
        let diff = Vec2.init(dx, dy)

        let diff = diff * 0.5f * movingRate

        if not <| nearEnough diff (Vec2.zero()) then
            this.UpdatePosition(diff)


    member private this.UpdatePosition(diff) =
        position <- this.Position + diff