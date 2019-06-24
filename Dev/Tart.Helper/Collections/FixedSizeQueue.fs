namespace wraikny.Tart.Helper.Collections

open System.Collections.Generic

/// Fixed Size (automatically dequeue) and Thread Safe (with lock)
type FixedSizeQueue<'T> = class
    val private Queue : Queue<'T>
    val Limit : int
    val private _Lock : System.Object

    /// Constructer with limit
    public new(limit : int) =
        {
            Queue = new Queue<'T>()
            Limit = limit
            _Lock = new System.Object()
        }


    /// Constructer from collection
    public new(collection : IEnumerable<'T>) =
        let queue = new Queue<'T>(collection)
        {
            Queue = queue
            Limit = queue.Count
            _Lock = new System.Object()
        }


    /// Get queue count with lock
    member public this.Count
        with get() =
            lock this._Lock <| fun _ ->
                this.Queue.Count

    
    /// Enqueue and dequeue while count > limit with lock
    member public this.Enqueue(o : 'T) =
        lock this._Lock <| fun _ ->
            this.Queue.Enqueue(o)
            while this.Queue.Count > this.Limit do
                this.Queue.Dequeue() |> ignore


    /// Dequeue with lock
    member public this.TryDequeue() =
        lock this._Lock <| fun _ ->
            if this.Queue.Count > 0 then
                Some <| this.Queue.Dequeue()
            else
                None

  
    /// Clear queue with lock
    member public this.Clear() =
        lock this._Lock <| fun _ ->
            this.Queue.Clear()


    /// Get Enumeartor with lock
    member this.GetEnumerator() =
        lock this._Lock <| fun _ ->
            (new List<'T>(this.Queue)).GetEnumerator()
            :> IEnumerator<'T>


    interface IReadOnlyCollection<'T> with
        member this.Count with get() = this.Count 

        member this.GetEnumerator() = this.GetEnumerator()

        member this.GetEnumerator() =
            this.GetEnumerator()
            :> System.Collections.IEnumerator

end