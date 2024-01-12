using System;
using System.Collections.Generic;

namespace Nu
{
    /// <summary>
    /// Concurrent priority queue.
    /// </summary>
    /// <typeparam name="P">Type of priority.</typeparam>
    /// <typeparam name="V">Type of element.</typeparam>
    public class ConcurrentPriorityQueue<P, V> : IPriorityQueue<P, V>
    {
        /// <summary>
        /// Enqueue an element.
        /// Thread-safe.
        /// </summary>
        public void Enqueue(P priority, V value)
        {
            lock (locker) queue.Enqueue(priority, value);
        }

        /// <summary>
        /// Attempt to dequeue any current element.
        /// Thread-safe.
        /// </summary>
        public bool TryDequeue(ref V value)
        {
            lock (locker) return queue.TryDequeue(ref value);
        }

        private readonly object locker = new object();
        private readonly PriorityQueue<P, V> queue = new PriorityQueue<P, V>();
    }
}
