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

                if this.IsCollidedWith other then
                    other.Move(this)
            else
                other.Move(this)

                if this.IsCollidedWith other then
                    this.Move(other)

        let d = 1.0f * movingRate * movingRate * count
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
            let centerOrder = sign ((axis this.Center) - (axis other.Center)) |> float32
            let diff =
                if centerOrder > 0.0f then
                    axis other.RightDown - axis this.Position
                else
                    axis other.Position - axis this.RightDown

            (abs(diff) + 1.0f / movingRate) * centerOrder

        let dx, dy = getDiff Vec2.x, getDiff Vec2.y

        let diff = Vec2.init <| if dx < dy then (dx, 0.0f) else (0.0f, dy)
        // let diff = Vec2.init(dx, dy)

        let diff = diff * movingRate

        this.UpdatePosition(diff)


    member private this.UpdatePosition(diff) =
        position <- this.Position + diff
        position <- position |> (Vec2.map int >> Vec2.map float32)