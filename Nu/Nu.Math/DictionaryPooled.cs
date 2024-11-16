using System;
using System.Collections.Generic;
using System.Runtime.CompilerServices;
using System.Threading;

namespace Nu
{
    /// <summary>
    /// A dictionary from a sychronized global dictionary pool.
    /// </summary>
    public class DictionaryPooled<K, V> : IDisposable
    {
        /// <summary>
        /// Create a pooled dictionary.
        /// </summary>
        public DictionaryPooled(Func<Dictionary<K,V>> create)
        {
            dict = Alloc(create);
        }

        /// <summary>
        /// The underlying pooled dictionary.
        /// Do NOT hold onto this past this object's life time!
        /// </summary>
        public Dictionary<K, V> Deref
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
        /// The underlying dictionary enumerator.
        /// Do NOT hold onto this past this object's life time!
        /// </summary>
        public Dictionary<K, V>.Enumerator GetEnumerator()
        {
            ThrowIfDisposed();
            return dict.GetEnumerator();
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
            var thatObjectPooled = that as DictionaryPooled<K, V>;
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
            Free(dict);
            GC.SuppressFinalize(this);
        }

        ~DictionaryPooled()
        {
            Free(dict);
        }

        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        private void ThrowIfDisposed()
        {
            if (Interlocked.CompareExchange(ref disposed, 0, 0) == 1)
                throw new ObjectDisposedException(GetType().FullName);
        }

        private readonly Dictionary<K, V> dict;
        private int disposed;

        private static Dictionary<K, V> Alloc(Func<Dictionary<K, V>> create)
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

        private static void Free(Dictionary<K, V> dict)
        {
            // clear
            dict.Clear();

            // transfer pools
            lock (poolLock)
                if (poolB.Remove(dict))
                    poolA.Add(dict);
        }

        private static readonly object poolLock = new object();
        private static readonly HashSet<Dictionary<K, V>> poolA = new HashSet<Dictionary<K, V>>();
        private static readonly HashSet<Dictionary<K, V>> poolB = new HashSet<Dictionary<K, V>>();
    }
}
