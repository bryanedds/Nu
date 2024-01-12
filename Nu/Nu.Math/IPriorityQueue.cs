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
        /// Enqueue an element with the given priority.
        /// </summary>
        void Enqueue(P priority, V value);

        /// <summary>
        /// Attempt to dequeue any current element.
        /// </summary>
        bool TryDequeue(ref V value);
    }
}