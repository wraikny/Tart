namespace wraikny.Tart.Advanced.Dungeon

open System.Collections.Generic
open System.Linq

open wraikny.Tart.Helper.Math
open wraikny.Tart.Helper.Geometry


[<Class>]
type internal MovingRoom(rect : float32 Rect2, movingRate, rooms) =
    let rooms : IReadOnlyList<MovingRoom> = rooms

    let movingRate : float32 = movingRate

    let size = rect.size
    let mutable position = rect.position
    let mutable lastPosition = position


    member this.Position with get() = position
    member this.RightDown with get() = position + size
    member this.Center with get() = position + size /. 2.0f

    member val IsMoving = true with get, set


    member this.Update(count) =
        let targets =
            rooms
                .Where(fun o -> o <> this)
                .Where(this.IsCollidedWith)

        lastPosition <- position

        for other in targets do
            let len (o : MovingRoom) = o.Center |> Vector.squaredLength
            if (len this) > (len other) then
                this.Move(other)
                other.Move(this)

            else
                other.Move(this)
                this.Move(other)


        let d = 0.5f * movingRate * movingRate * count
        this.IsMoving <- Vector.squaredLength (lastPosition - position) > d * d
    


    member this.RectF
        with get() : float32 Rect2 = {
            position = this.Position
            size = size
        }

    member this.RectI
        with get() : int Rect2 = this.RectF |> Rect.mapVec int


    member private this.IsCollidedWith(other : MovingRoom) =
        Rect.isCollided this.RectF other.RectF


    member this.Move(other : MovingRoom) =

        let getDiff axis =
            let centerOrder = ((axis this.Center) - (axis other.Center)) |> sign |> float32
            let diff =
                if centerOrder > 0.0f then
                    axis other.RightDown - axis this.Position
                else
                    axis other.Position - axis this.RightDown

            diff

        let dx, dy = getDiff Vec2.x, getDiff Vec2.y

        let diff = Vec2.init <| if abs dx < abs dy then (dx, 0.0f) else (0.0f, dy)
        // let diff = Vec2.init(dx, dy)

        let diff = diff *. movingRate

        this.UpdatePosition(diff)
        // this.UpdatePosition(this.Center |> Vec2.normalize)


    member private this.UpdatePosition(diff) =
        position <- (position + diff) |> Vec2.map floor