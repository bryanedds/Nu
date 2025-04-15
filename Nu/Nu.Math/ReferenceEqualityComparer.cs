using System;
using System.Collections;
using System.Collections.Generic;
using System.Runtime.CompilerServices;

namespace Nu
{
    /// <summary>
    /// A simple reference equality comparer for use in C# (F# uses HashIdentity.Reference).
    /// </summary>
    public sealed class ReferenceEqualityComparer : IEqualityComparer<object>, IEqualityComparer
    {
        private ReferenceEqualityComparer() { }
        public static ReferenceEqualityComparer Instance { get; } = new ReferenceEqualityComparer();
        public new bool Equals(object x, object y) => ReferenceEquals(x, y);
        public int GetHashCode(object obj) => RuntimeHelpers.GetHashCode(obj!);
    }
}
