using System;
using System.Collections.Generic;
using System.Linq;

namespace Nu
{
    /// <summary>
    /// Priority queue.
    /// </summary>
    /// <typeparam name="P">Type of priority.</typeparam>
    /// <typeparam name="V">Type of element.</typeparam>
    public class PriorityQueue<P, V> : IPriorityQueue<P, V>
    {
        /// <summary>
        /// Check that the queue is empty.
        /// </summary>
        public bool IsEmpty
        {
            get { return !list.Any(); }
        }

        /// <summary>
        /// Enqueue an item.
        /// </summary>
        public void Enqueue(P priority, V value)
        {
            Queue<V> q;
            if (!list.TryGetValue(priority, out q))
            {
                q = new Queue<V>();
                list.Add(priority, q);
            }
            q.Enqueue(value);
        }

        /// <summary>
        /// Dequeue the current item, throwing if none exists.
        /// </summary>
        public V Dequeue()
        {
            // will throw if there isn’t any first item!
            var pair = list.First();
            var v = pair.Value.Dequeue();
            if (pair.Value.Count == 0) // nothing left of the top priority.
                list.Remove(pair.Key);
            return v;
        }

        private readonly SortedDictionary<P, Queue<V>> list = new SortedDictionary<P, Queue<V>>();
    }
}
