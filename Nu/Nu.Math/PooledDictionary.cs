using System;
using System.Collections.Generic;
using System.Runtime.CompilerServices;
using System.Threading;

namespace Nu
{
    /// <summary>
    /// A dictionary from a sychronized global dictionary pool.
    /// </summary>
    public class PooledDictionary<D, K, V> : IDisposable where D : IDictionary<K, V>
    {
        /// <summary>
        /// Create a pooled dictionary.
        /// </summary>
        public PooledDictionary(Func<D> create)
        {
            dict = Alloc(create);
        }

        /// <summary>
        /// The underlying pooled dictionary.
        /// Do NOT hold onto this past this object's life time!
        /// </summary>
        public D Deref
        {
            get
            {
                ThrowIfDisposed();
                return dict;
            }
        }

        /// <summary>
        /// Get the value stored with the given key.
        /// </summary>
        public V this[K key]
        {
            get
            {
                ThrowIfDisposed();
                return dict[key];
            }
            set
            {
                ThrowIfDisposed();
                dict[key] = value;
            }
        }

        /// <summary>
        /// Number of items in the dictionary.
        /// </summary>
        public int Count
        {
            get
            {
                ThrowIfDisposed();
                return dict.Count;
            }
        }

        /// <summary>
        /// Attempt to get the value associated with the given key.
        /// </summary>
        public bool TryGetValue(K key, out V value)
        {
            ThrowIfDisposed();
            return dict.TryGetValue(key, out value);
        }

        /// <summary>
        /// Add a value.
        /// </summary>
        public void Add(K key, V value)
        {
            ThrowIfDisposed();
            dict.Add(key, value);
        }

        /// <summary>
        /// Remove a value.
        /// </summary>
        public void Remove(K key)
        {
            ThrowIfDisposed();
            dict.Remove(key);
        }

        /// <summary>
        /// Check that dictionary contain the given key.
        /// </summary>
        public bool ContainsKey(K key)
        {
            ThrowIfDisposed();
            return dict.ContainsKey(key);
        }

        /// <summary>
        /// Clear the dictionary.
        /// </summary>
        public void Clear()
        {
            ThrowIfDisposed();
            dict.Clear();
        }

        /// <summary>
        /// Clone.
        /// </summary>
        public PooledDictionary<D, K, V> Clone(Func<D> create)
        {
            var dict = new PooledDictionary<D, K, V>(create);
            var deref = dict.Deref;
            foreach (var entry in this.dict) deref.Add(entry.Key, entry.Value);
            return dict;
        }

        /// <summary>
        /// Hashing.
        /// </summary>
        public override int GetHashCode()
        {
            ThrowIfDisposed();
            return dict.GetHashCode();
        }

        /// <summary>
        /// Equality.
        /// </summary>
        public override bool Equals(object that)
        {
            if (that == null) return false;
            ThrowIfDisposed();
            var thatObjectPooled = that as PooledDictionary<D, K, V>;
            return dict.Equals(thatObjectPooled.dict);
        }

        /// <summary>
        /// Stringization.
        /// </summary>
        public override string ToString()
        {
            ThrowIfDisposed();
            return dict.ToString();
        }

        /// <summary>
        /// Disposal.
        /// </summary>
        public void Dispose()
        {
            if (Interlocked.CompareExchange(ref disposed, 1, 0) == 0)
            {
                Free(dict);
                GC.SuppressFinalize(this);
            }
        }

        ~PooledDictionary()
        {
            Free(dict);
        }

        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        private void ThrowIfDisposed()
        {
            if (Interlocked.CompareExchange(ref disposed, 0, 0) == 1)
            {
                throw new ObjectDisposedException(GetType().FullName);
            }
        }

        private readonly D dict;
        private volatile int disposed;

        private static D Alloc(Func<D> create)
        {
            lock (poolLock)
            {
                // add dict if missing
                if (poolA.Count == 0)
                    poolA.Add(create());

                // allocate dict
                var enr = poolA.GetEnumerator();
                enr.MoveNext();
                var dict = enr.Current;
                poolA.Remove(dict);
                poolB.Add(dict);
                return dict;
            }
        }

        private static void Free(D dict)
        {
            // clear
            dict.Clear();

            // transfer pools
            lock (poolLock)
                if (poolB.Remove(dict))
                    poolA.Add(dict);
        }

        private static readonly object poolLock = new object();
        private static readonly HashSet<D> poolA = new HashSet<D>();
        private static readonly HashSet<D> poolB = new HashSet<D>();
    }
}
