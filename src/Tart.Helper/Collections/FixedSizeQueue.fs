namespace wraikny.Tart.Helper.Collections

open System.Collections.Generic


open wraikny.Tart.Helper.Utils

/// Fixed Size (automatically dequeue) and Thread Safe (with lock)
type FixedSizeQueue<'T> private(queue, limit) =
    let queue : Queue<'T> = queue
    let limit : int = limit
    let _lock = System.Object()

    /// Constructer with limit
    public new(limit : int) =
        new FixedSizeQueue<'T>(new Queue<'T>(), limit)


    /// Constructer from collection
    public new(collection : IEnumerable<'T>) =
        let queue = new Queue<'T>(collection)
        new FixedSizeQueue<'T>(queue, queue.Count)

    member private __.Lock(f) =
        lock _lock <| fun _ -> f()


    member __.Limit with get() = limit


    /// Get queue count with lock
    member public this.Count
        with get() =
            this.Lock <| fun _ ->
                queue.Count

    /// Clear queue with lock
    member public this.Clear() =
        this.Lock <| fun _ ->
            queue.Clear()

    /// Get Enumeartor with lock
    member this.GetEnumerator() =
        this.Lock <| fun _ ->
            (new List<'T>(queue)).GetEnumerator()
            :> IEnumerator<'T>

    /// Enqueue and dequeue while count > limit with lock
    member this.Enqueue(o : 'T) =
        this.Lock <| fun _ ->
            queue.Enqueue(o)
            while queue.Count > this.Limit do
                queue.Dequeue() |> ignore


    /// Dequeue with lock
    member this.TryDequeue() =
        this.Lock <| fun _ ->
            if queue.Count > 0 then
                Some <| queue.Dequeue()
            else
                None

    interface IQueue<'T> with
        member this.Enqueue(x) = this.Enqueue(x)
        member this.TryDequeue() = this.TryDequeue()

        member this.Count with get() = this.Count 

        member this.GetEnumerator() = this.GetEnumerator()

        member this.GetEnumerator() =
            this.GetEnumerator()
            :> System.Collections.IEnumerator

        member this.Clear() = this.Clear()