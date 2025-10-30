using System;

namespace System.Collections.Generic
{
    /// <summary>
    /// Concurrent priority queue.
    /// </summary>
    /// <typeparam name="V">Type of element.</typeparam>
    /// <typeparam name="P">Type of priority.</typeparam>
    public class ConcurrentPriorityQueue<V, P>
    {
        /// <summary>
        /// Enqueue an element.
        /// Thread-safe.
        /// </summary>
        public void Enqueue(V value, P priority)
        {
            lock (locker) queue.Enqueue(value, priority);
        }

        /// <summary>
        /// Attempt to dequeue any current element.
        /// Thread-safe.
        /// </summary>
        public bool TryDequeue(out V element, out P priority)
        {
            lock (locker) return queue.TryDequeue(out element, out priority);
        }

        /// <summary>
        /// Clear the queue.
        /// Thread-safe.
        /// </summary>
        public void Clear()
        {
            lock (locker) queue.Clear();
        }

        private readonly object locker = new object();
        private readonly PriorityQueue<V, P> queue = new PriorityQueue<V, P>();
    }
}
