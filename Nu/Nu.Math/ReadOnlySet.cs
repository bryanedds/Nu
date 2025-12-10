using System;

namespace System.Collections.Generic
{
    public static class ISetExtension
    {
        public static ReadOnlySet<T> AsReadOnly<T>(this ISet<T> set)
        {
            return new ReadOnlySet<T>(set);
        }
    }

    public struct ReadOnlySet<T> : IReadOnlyCollection<T>, ISet<T>
    {
        public ReadOnlySet(ISet<T> set)
        {
            this.set = set;
        }

        public readonly IEnumerator<T> GetEnumerator()
        {
            return set.GetEnumerator();
        }

        readonly IEnumerator IEnumerable.GetEnumerator()
        {
            return ((IEnumerable)set).GetEnumerator();
        }

        void ICollection<T>.Add(T item)
        {
            throw new NotSupportedException("Set is a read only set.");
        }

        public void UnionWith(IEnumerable<T> other)
        {
            throw new NotSupportedException("Set is a read only set.");
        }

        public void IntersectWith(IEnumerable<T> other)
        {
            throw new NotSupportedException("Set is a read only set.");
        }

        public void ExceptWith(IEnumerable<T> other)
        {
            throw new NotSupportedException("Set is a read only set.");
        }

        public void SymmetricExceptWith(IEnumerable<T> other)
        {
            throw new NotSupportedException("Set is a read only set.");
        }

        public readonly bool IsSubsetOf(IEnumerable<T> other)
        {
            return set.IsSubsetOf(other);
        }

        public readonly bool IsSupersetOf(IEnumerable<T> other)
        {
            return set.IsSupersetOf(other);
        }

        public readonly bool IsProperSupersetOf(IEnumerable<T> other)
        {
            return set.IsProperSupersetOf(other);
        }

        public readonly bool IsProperSubsetOf(IEnumerable<T> other)
        {
            return set.IsProperSubsetOf(other);
        }

        public readonly bool Overlaps(IEnumerable<T> other)
        {
            return set.Overlaps(other);
        }

        public readonly bool SetEquals(IEnumerable<T> other)
        {
            return set.SetEquals(other);
        }

        public bool Add(T item)
        {
            throw new NotSupportedException("Set is a read only set.");
        }

        public void Clear()
        {
            throw new NotSupportedException("Set is a read only set.");
        }

        public readonly bool Contains(T item)
        {
            return set.Contains(item);
        }

        public readonly void CopyTo(T[] array, int arrayIndex)
        {
            set.CopyTo(array, arrayIndex);
        }

        public bool Remove(T item)
        {
            throw new NotSupportedException("Set is a read only set.");
        }

        public readonly int Count
        {
            get { return set.Count; }
        }

        public readonly bool IsReadOnly
        {
            get { return true; }
        }

        private readonly ISet<T> set;
    }
}
