using System;
using System.Collections;
using System.Collections.Generic;
using System.Runtime.CompilerServices;
using System.Threading;

namespace Nu
{
    /// <summary>
    /// An array from a sychronized global array pool.
    /// </summary>
    public class PooledArray<T> : IDisposable, IEnumerable<T>
    {
        /// <summary>
        /// Create a pooled array.
        /// </summary>
        public PooledArray(int length, bool clearOnFree)
        {
            array = Alloc(length);
            this.clearOnFree = clearOnFree;
        }

        /// <summary>
        /// Index an array element.
        /// </summary>
        public ref T this[int index]
        {
            get
            {
                ThrowIfDisposed();
                return ref array[index];
            }
        }

        /// <summary>
        /// The length of the pooled array.
        /// </summary>
        public int Length
        {
            get
            {
                ThrowIfDisposed();
                return array.Length;
            }
        }

        /// <summary>
        /// The underlying pooled array.
        /// Do NOT hold onto this past this object's life time!
        /// </summary>
        public T[] Deref
        {
            get
            {
                ThrowIfDisposed();
                return array;
            }
        }

        /// <summary>
        /// Clone the pooled array.
        /// </summary>
        public PooledArray<T> Clone()
        {
            ThrowIfDisposed();
            var arr = new PooledArray<T>(array.Length, clearOnFree);
            array.CopyTo(arr.array, 0);
            return arr;
        }

        /// <summary>
        /// Hashing.
        /// </summary>
        public override int GetHashCode()
        {
            ThrowIfDisposed();
            return array.GetHashCode();
        }

        /// <summary>
        /// Equality.
        /// </summary>
        public override bool Equals(object that)
        {
            ThrowIfDisposed();
            var thatArrayPooled = that as PooledArray<T>;
            return array == thatArrayPooled.array;
        }

        /// <summary>
        /// Stringization.
        /// </summary>
        public override string ToString()
        {
            ThrowIfDisposed();
            return array.ToString();
        }

        /// <summary>
        /// Generic enumeration.
        /// Do NOT hold onto this past this object's life time!
        /// </summary>
        public IEnumerator<T> GetEnumerator()
        {
            ThrowIfDisposed();
            return ((IEnumerable<T>)array).GetEnumerator();
        }

        /// <summary>
        /// General enumeration.
        /// Do NOT hold onto this past this object's life time!
        /// </summary>
        IEnumerator IEnumerable.GetEnumerator()
        {
            ThrowIfDisposed();
            return array.GetEnumerator();
        }

        /// <summary>
        /// Disposal.
        /// </summary>
        public void Dispose()
        {
            if (Interlocked.CompareExchange(ref disposed, 1, 0) == 0)
            {
                Free(array, clearOnFree);
                GC.SuppressFinalize(this);
            }
        }

        ~PooledArray()
        {
            Free(array, clearOnFree);
        }

        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        private void ThrowIfDisposed()
        {
            if (Interlocked.CompareExchange(ref disposed, 0, 0) == 1)
            {
                throw new ObjectDisposedException(GetType().FullName);
            }
        }

        private readonly T[] array;
        private readonly bool clearOnFree;
        private volatile int disposed;

        private static T[] Alloc(int length)
        {
            lock (poolLock)
            {
                // add arr if missing
                var (poolA, poolB) = GetPools(length);
                if (poolA.Count == 0) poolA.Add(new T[length]);

                // allocate arr
                var enr = poolA.GetEnumerator();
                enr.MoveNext();
                var arr = enr.Current;
                poolA.Remove(arr);
                poolB.Add(arr);
                return arr;
            }
        }

        private static (HashSet<T[]>, HashSet<T[]>) GetPools(int length)
        {
            // add pool A if missing
            if (!poolsA.TryGetValue(length, out var poolA))
            {
                poolA = new HashSet<T[]>(ReferenceEqualityComparer.Instance);
                poolsA.Add(length, poolA);
            }

            // add pool B if missing
            if (!poolsB.TryGetValue(length, out var poolB))
            {
                poolB = new HashSet<T[]>(ReferenceEqualityComparer.Instance);
                poolsB.Add(length, poolB);
            }

            return (poolA, poolB);
        }

        private static void Free(T[] arr, bool clear)
        {
            // clear when requested
            if (clear)
            {
                for (int i = 0; i < arr.Length; ++i)
                {
                    arr[i] = default(T);
                }
            }

            // transform pools
            lock (poolLock)
            {
                var length = arr.Length;
                var (poolA, poolB) = GetPools(length);
                if (poolB.Remove(arr)) poolA.Add(arr);
            }
        }

        private static readonly object poolLock = new object();
        private static readonly Dictionary<int, HashSet<T[]>> poolsA = new Dictionary<int, HashSet<T[]>>();
        private static readonly Dictionary<int, HashSet<T[]>> poolsB = new Dictionary<int, HashSet<T[]>>();
    }
}
