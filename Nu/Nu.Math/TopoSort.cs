using System;
using System.Collections.Generic;
using System.Collections.ObjectModel;
using System.Linq;

namespace Nu
{
    /// <summary>
    /// Sourced from - https://www.codeproject.com/Articles/869059/Topological-sorting-in-Csharp
    /// </summary>
    public static class TopologicalSort
    {
        public static (bool, IList<ICollection<T>>) Group<T, TKey>(this IEnumerable<T> source, Func<T, IEnumerable<TKey>> getDependencies, Func<T, TKey> getKey)
        {
            ICollection<T> source2 = (source as ICollection<T>) ?? source.ToArray();
            return Group<T>(source2, RemapDependencies(source2, getDependencies, getKey), null);
        }

        public static (bool, IList<ICollection<T>>) Group<T, TKey>(this IEnumerable<T> source, Func<T, IEnumerable<T>> getDependencies, Func<T, TKey> getKey)
        {
            return Group(source, getDependencies, new GenericEqualityComparer<T, TKey>(getKey));
        }

        public static (bool, IList<ICollection<T>>) Group<T>(this IEnumerable<T> source, Func<T, IEnumerable<T>> getDependencies, IEqualityComparer<T> comparer = null)
        {
            var cycleFound = false;
            var sorted = new List<ICollection<T>>();
            var visited = new Dictionary<T, int>(comparer);
            foreach (var item in source)
            {
                var (cycleFound2, _) = Visit(item, getDependencies, sorted, visited);
                cycleFound = cycleFound || cycleFound2;
            }
            return (cycleFound, sorted);
        }

        public static IList<T> Sort<T, TKey>(this IEnumerable<T> source, Func<T, IEnumerable<TKey>> getDependencies, Func<T, TKey> getKey)
        {
            ICollection<T> source2 = (source as ICollection<T>) ?? source.ToArray();
            return Sort<T>(source2, RemapDependencies(source2, getDependencies, getKey), null);
        }

        public static IList<T> Sort<T, TKey>(this IEnumerable<T> source, Func<T, IEnumerable<T>> getDependencies, Func<T, TKey> getKey)
        {
            return Sort(source, getDependencies, new GenericEqualityComparer<T, TKey>(getKey));
        }

        public static IList<T> Sort<T>(this IEnumerable<T> source, Func<T, IEnumerable<T>> getDependencies, IEqualityComparer<T> comparer = null)
        {
            var sorted = new List<T>();
            var visited = new Dictionary<T, bool>(comparer);
            foreach (var item in source)
                Visit(item, getDependencies, sorted, visited);
            return sorted;
        }

        private static (bool, int) Visit<T>(T item, Func<T, IEnumerable<T>> getDependencies, List<ICollection<T>> sorted, Dictionary<T, int> visited)
        {
            const int inProcess = -1;
			var cycleFound = false;
			var alreadyVisited = visited.TryGetValue(item, out int level);

            if (alreadyVisited)
            {
                if (level == inProcess) cycleFound = true;
            }
            else
            {
                visited[item] = (level = inProcess);
                var dependencies = getDependencies(item);
                if (dependencies != null)
                {
                    foreach (var dependency in dependencies)
                    {
                        var (cycleFound2, depLevel) = Visit(dependency, getDependencies, sorted, visited);
                        if (cycleFound2) cycleFound = true;
                        level = System.Math.Max(level, depLevel);
                    }
                }

                visited[item] = ++level;
                while (sorted.Count <= level)
                    sorted.Add(new Collection<T>());
                sorted[level].Add(item);
            }

            return (cycleFound, level);
        }

        private static bool Visit<T>(T item, Func<T, IEnumerable<T>> getDependencies, List<T> sorted, Dictionary<T, bool> visited)
        {
            var cycleFound = false;
            var alreadyVisited = visited.TryGetValue(item, out bool inProcess);
            if (alreadyVisited)
            {
                if (inProcess) cycleFound = true;
            }
            else
            {
                visited[item] = true;
                var dependencies = getDependencies(item);
                if (dependencies != null)
                    foreach (var dependency in dependencies)
                        Visit(dependency, getDependencies, sorted, visited);
                visited[item] = false;
                sorted.Add(item);
            }
            return cycleFound;
        }

        private static Func<T, IEnumerable<T>> RemapDependencies<T, TKey>(IEnumerable<T> source, Func<T, IEnumerable<TKey>> getDependencies, Func<T, TKey> getKey)
        {
            var map = source.ToDictionary(getKey);
            return item =>
            {
                var dependencies = getDependencies(item);
                return dependencies?.Select(key => map[key]);
            };
        }

        private class GenericEqualityComparer<TItem, TKey> : EqualityComparer<TItem>
        {
            private readonly Func<TItem, TKey> getKey;
            private readonly EqualityComparer<TKey> keyComparer;

            public GenericEqualityComparer(Func<TItem, TKey> getKey)
            {
                this.getKey = getKey;
                keyComparer = EqualityComparer<TKey>.Default;
            }

            public override bool Equals(TItem x, TItem y)
            {
                if (x == null && y == null) return true;
                if (x == null || y == null) return false;
                return keyComparer.Equals(getKey(x), getKey(y));
            }

            public override int GetHashCode(TItem obj)
            {
                if (obj == null) return 0;
                return keyComparer.GetHashCode(getKey(obj));
            }
        }
    }
}
