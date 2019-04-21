namespace wraikny.Tart.Helper.Wrapper

open System.Collections.Generic

type private FixedSizeQueueParam<'T> =
    | Limit of int
    | Collection of IEnumerable<'T>


/// Fixed Size (automatically dequeue) and Thread Safe (with lock)
[<Class>]
type FixedSizeQueue<'T> private (p) =
    let queue, limit = p |> function
        | Limit limit ->
            new Queue<'T>(), limit
        | Collection collection ->
            let queue =
                new Queue<'T>(collection)
            queue, queue.Count

    /// puseudo for lock
    let _lock = new System.Object()


    /// Constructer with limit
    public new(limit : int) =
        new FixedSizeQueue<'T>(Limit limit)


    /// Constructer from collection
    public new(collection : IEnumerable<'T>) =
        new FixedSizeQueue<'T>(Collection collection)


    /// Limit of queue length
    member val Limit = limit with get


    /// Get queue count with lock
    member public __.Count
        with get() =
            lock _lock <| fun _ ->
                queue.Count

    
    /// Enqueue and dequeue while count > limit with lock
    member public this.Enqueue(o : 'T) =
        lock _lock <| fun _ ->
            queue.Enqueue(o)
            while queue.Count > this.Limit do
                queue.Dequeue() |> ignore


    /// Dequeue with lock
    member public __.TryDequeue() =
        lock _lock <| fun _ ->
            if queue.Count > 0 then
                Some <| queue.Dequeue()
            else
                None

  
    /// Clear queue with lock
    member public __.Clear() =
        lock _lock <| fun _ ->
            queue.Clear()


    /// Get Enumeartor with lock
    member __.GetEnumerator() =
        lock _lock <| fun _ ->
            (new List<'T>(queue)).GetEnumerator()
            :> IEnumerator<'T>


    interface IReadOnlyCollection<'T> with
        member this.Count
            with get() = this.Count 
        member this.GetEnumerator() = this.GetEnumerator()
        member this.GetEnumerator() =
            this.GetEnumerator()
            :> System.Collections.IEnumerator
