using System;
using System.Collections;
using System.Collections.Generic;
using System.Linq;
using System.Runtime.CompilerServices;
using System.Threading;

namespace Nu
{
    /// <summary>
    /// An array from a sychronized global array pool.
    /// </summary>
    public class ArrayPooled<T> : IDisposable, IEnumerable<T>
    {
        /// <summary>
        /// Create a pooled array.
        /// </summary>
        public ArrayPooled(int length, bool clearOnFree)
        {
            array = Alloc(length);
            this.clearOnFree = clearOnFree;
            this.length = length;
        }

        /// <summary>
        /// Index an array element.
        /// </summary>
        public ref T this[int index]
        {
            get
            {
                ThrowIfDisposed();
                if (index >= length) throw new ArgumentOutOfRangeException(nameof(index));
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
                return length;
            }
        }

        /// <summary>
        /// The underlying pooled array.
        /// Do NOT hold onto this past this object's life time!
        /// </summary>
        public T[] Array
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
        public ArrayPooled<T> Clone()
        {
            ThrowIfDisposed();
            var arr = new ArrayPooled<T>(length, clearOnFree);
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
            var thatArrayPooled = that as ArrayPooled<T>;
            return
                length == thatArrayPooled.length &&
                array == thatArrayPooled.array;
        }

        /// <summary>
        /// Stringization.
        /// </summary>
        public override string ToString()
        {
            return array.ToString();
        }

        /// <summary>
        /// Generic enumeration.
        /// </summary>
        public IEnumerator<T> GetEnumerator()
        {
            return ((IEnumerable<T>)array).GetEnumerator();
        }

        /// <summary>
        /// General enumeration.
        /// </summary>
        IEnumerator IEnumerable.GetEnumerator()
        {
            return array.GetEnumerator();
        }

        /// <summary>
        /// Disposal.
        /// </summary>
        public void Dispose()
        {
            Free(array, clearOnFree);
            GC.SuppressFinalize(this);
        }

        ~ArrayPooled()
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
        private readonly int length;
        private int disposed;

        private static T[] Alloc(int length)
        {
            lock (poolLock)
            {
                // add arr if missing
                var (poolA, poolB) = GetPools(length);
                if (poolA.Count == 0) poolA.Add(new T[length]);

                // allocate arr
                var arr = poolA.First();
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
                var comparer = ReferenceEqualityComparer.Instance;
                poolA = new HashSet<T[]>();
                poolsA.Add(length, poolA);
            }

            // add pool B if missing
            if (!poolsB.TryGetValue(length, out var poolB))
            {
                var comparer = ReferenceEqualityComparer.Instance;
                poolB = new HashSet<T[]>();
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

            // lock
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
