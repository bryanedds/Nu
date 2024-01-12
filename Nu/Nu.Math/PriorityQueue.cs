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
        /// Enqueue an element with the given priority.
        /// </summary>
        public void Enqueue(P priority, V value)
        {
            Queue<V> q;
            if (!elements.TryGetValue(priority, out q))
            {
                q = new Queue<V>();
                elements.Add(priority, q);
            }
            q.Enqueue(value);
        }

        /// <summary>
        /// Attempt to dequeue any current element.
        /// </summary>
        public bool TryDequeue(ref V value)
        {
            if (elements.Count > 0)
            {
                var pair = elements.First();
                value = pair.Value.Dequeue();
                if (pair.Value.Count == 0) elements.Remove(pair.Key);
                return true;
            }
            return false;
        }

        private readonly SortedDictionary<P, Queue<V>> elements = new SortedDictionary<P, Queue<V>>();
    }
}
