namespace wraikny.Tart.Advanced.Dungeon

open System.Collections.Generic
open System.Linq

open wraikny.Tart.Helper.Math
open wraikny.Tart.Helper.Geometry


[<Class>]
type private MovingRoom(rect : float32 Rect, movingRate, rooms) =
    let rooms : IReadOnlyList<MovingRoom> = rooms

    let movingRate : float32 = movingRate

    let position = ref rect.position

    let rightDown = ref (rect |> Rect.diagonalPosition)

    let len (o : MovingRoom) = o.Position |> Vec2.squaredLength

    member val Position = !position with get

    member val RightDown = !rightDown with get

    member val Size = rect.size |> Vec2.map int with get

    member val IsMoving = false with get, set


    member this.Update() =
        let targets =
            rooms
                .Where(fun o -> this.IsCollidedWith o)
                .Where(fun o -> o <> this)

        for other in targets do
            if (len this) > (len other) then
                this.Move(other)
                other.Move(this)
            else
                other.Move(this)
                this.Move(other)

        this.IsMoving <- targets.Count() > 0
    

    member this.RectI
        with get() : int Rect = {
            position = this.Position |> Vec2.map int
            size = this.Size
        }

    member private this.IsCollidedWith(other : MovingRoom) =
        Rect.isCollided this.RectI other.RectI


    member this.Move(other : MovingRoom) =
        let getDiff axis =
            if (axis this.Position < axis other.Position) then
                axis other.RightDown - axis this.Position
            else
                axis this.RightDown - axis other.Position

        let dx, dy = getDiff Vec2.x, getDiff Vec2.y

        let diff = Vec2.init <| if dx < dy then (dx, 0.0f) else (0.0f, dy)
        // let diff = Vec2.init(dx, dy)

        let diff = diff * 0.5f * movingRate

        this.UpdatePosition(diff)


    member private this.UpdatePosition(diff) =
        position := diff + this.Position
        rightDown := !rightDown + diff