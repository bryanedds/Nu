using System;
using System.Collections.Generic;
using System.Runtime.CompilerServices;
using System.Threading;

namespace Nu
{
    /// <summary>
    /// A collection from a sychronized global collection pool.
    /// Note that you'll have to Deref in order to enumerate this efficiently.
    /// </summary>
    public class PooledCollection<C, T> : IDisposable where C : ICollection<T>
    {
        /// <summary>
        /// Create a pooled collection.
        /// </summary>
        public PooledCollection(Func<C> create)
        {
            coll = Alloc(create);
        }

        /// <summary>
        /// The underlying pooled collection.
        /// Do NOT hold onto this past this object's life time!
        /// </summary>
        public C Deref
        {
            get
            {
                ThrowIfDisposed();
                return coll;
            }
        }

        /// <summary>
        /// Number of items in the collection.
        /// </summary>
        public int Count
        {
            get
            {
                ThrowIfDisposed();
                return coll.Count;
            }
        }

        /// <summary>
        /// Whether the collection is read-only.
        /// </summary>
        public bool IsReadOnly
        {
            get
            {
                ThrowIfDisposed(); 
                return coll.IsReadOnly;
            }
        }

        /// <summary>
        /// Add an item.
        /// </summary>
        public void Add(T item)
        {
            ThrowIfDisposed();
            coll.Add(item);
        }

        /// <summary>
        /// Remove an item.
        /// </summary>
        public void Remove(T item)
        {
            ThrowIfDisposed();
            coll.Remove(item);
        }

        /// <summary>
        /// Check that collection contain the given item.
        /// </summary>
        public bool Contains(T item)
        {
            ThrowIfDisposed();
            return coll.Contains(item);
        }

        /// <summary>
        /// Clear the collection.
        /// </summary>
        public void Clear()
        {
            ThrowIfDisposed();
            coll.Clear();
        }

        /// <summary>
        /// Copy the collection starting at the given array index.
        /// </summary>
        public void CopyTo(T[] array, int arrayIndex)
        {
            ThrowIfDisposed();
            coll.CopyTo(array, arrayIndex);
        }

        /// <summary>
        /// Clone.
        /// </summary>
        public PooledCollection<C, T> Clone(Func<C> create)
        {
            var coll = new PooledCollection<C, T>(create);
            foreach (var item in this.coll) coll.Add(item);
            return coll;
        }

        /// <summary>
        /// Hashing.
        /// </summary>
        public override int GetHashCode()
        {
            ThrowIfDisposed();
            return coll.GetHashCode();
        }

        /// <summary>
        /// Equality.
        /// </summary>
        public override bool Equals(object that)
        {
            if (that == null) return false;
            ThrowIfDisposed();
            var thatObjectPooled = that as PooledCollection<C, T>;
            return coll.Equals(thatObjectPooled.coll);
        }

        /// <summary>
        /// Stringization.
        /// </summary>
        public override string ToString()
        {
            ThrowIfDisposed();
            return coll.ToString();
        }

        /// <summary>
        /// Disposal.
        /// </summary>
        public void Dispose()
        {
            if (Interlocked.CompareExchange(ref disposed, 1, 0) == 0)
            {
                Free(coll);
                GC.SuppressFinalize(this);
            }
        }

        ~PooledCollection()
        {
            Free(coll);
        }

        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        private void ThrowIfDisposed()
        {
            if (Interlocked.CompareExchange(ref disposed, 0, 0) == 1)
            {
                throw new ObjectDisposedException(GetType().FullName);
            }
        }

        private readonly C coll;
        private volatile int disposed;

        private static C Alloc(Func<C> create)
        {
            lock (poolLock)
            {
                // add coll if missing
                if (poolA.Count == 0)
                    poolA.Add(create());

                // allocate coll
                var enr = poolA.GetEnumerator();
                enr.MoveNext();
                var coll = enr.Current;
                poolA.Remove(coll);
                poolB.Add(coll);
                return coll;
            }
        }

        private static void Free(C coll)
        {
            // clear
            coll.Clear();

            // transfer pools
            lock (poolLock)
                if (poolB.Remove(coll))
                    poolA.Add(coll);
        }

        private static readonly object poolLock = new object();
        private static readonly HashSet<C> poolA = new HashSet<C>();
        private static readonly HashSet<C> poolB = new HashSet<C>();
    }
}
