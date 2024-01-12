namespace Nu
{
    /// <summary>
    /// Priority queue interface.
    /// </summary>
    /// <typeparam name="P">Type of priority.</typeparam>
    /// <typeparam name="V">Type of element.</typeparam>
    public interface IPriorityQueue<P, V>
    {
        /// <summary>
        /// Check that the queue is empty.
        /// </summary>
        bool IsEmpty { get; }

        /// <summary>
        /// Dequeue the current item, throwing if none exists.
        /// </summary>
        V Dequeue();

        /// <summary>
        /// Enqueue an item.
        /// </summary>
        void Enqueue(P priority, V value);
    }
}