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
        /// Check that the queue is empty.
        /// Thread-safe.
        /// </summary>
        public bool IsEmpty
        {
            get { lock (locker) return queue.IsEmpty; }
        }

        /// <summary>
        /// Dequeue the current item, throwing if none exists.
        /// Thread-safe.
        /// </summary>
        public void Enqueue(P priority, V value)
        {
            lock (locker) queue.Enqueue(priority, value);
        }

        /// <summary>
        /// Enqueue an item.
        /// Thread-safe.
        /// </summary>
        public V Dequeue()
        {
            lock (locker) return queue.Dequeue();
        }

        private readonly object locker = new object();
        private readonly PriorityQueue<P, V> queue = new PriorityQueue<P, V>();
        private readonly SortedDictionary<P, Queue<V>> list = new SortedDictionary<P, Queue<V>>();
    }
}
