using System;
using System.Collections;
using System.Collections.Generic;

namespace Nu
{
    // AStar implementation as scraped from -
    // http://blogs.msdn.com/b/ericlippert/archive/2007/10/10/path-finding-using-a-in-c-3-0-part-four.aspx
    //
    // If this implementation turns out to work poorly, there's also this one -
    // http://ffogd.blogspot.com/2011/02/f-a-star-algorithm-with-priority-queue.html
    //
    // I was considering using QuickGraph, but it hasn't been maintained in years and doesn't work
    // on the Mono runtime, apparently due to strong-naming issues -
    // http://quickgraph.codeplex.com/workitem/25587

    public class Path<Node> : IEnumerable<Node>
    {
        public Node LastStep { get; private set; }
        public Path<Node> PreviousSteps { get; private set; }
        public float TotalCost { get; private set; }

        private Path(Node lastStep, Path<Node> previousSteps, float totalCost)
        {
            LastStep = lastStep;
            PreviousSteps = previousSteps;
            TotalCost = totalCost;
        }

        public Path(Node start) : this(start, null, 0) { }

        public Path<Node> AddStep(Node step, float stepCost)
        {
            return new Path<Node>(step, this, TotalCost + stepCost);
        }

        public IEnumerator<Node> GetEnumerator()
        {
            for (Path<Node> p = this; p != null; p = p.PreviousSteps)
            {
                yield return p.LastStep;
            }
        }

        IEnumerator IEnumerable.GetEnumerator()
        {
            return this.GetEnumerator();
        }
    }

    public interface Neighborable<N>
    {
        IEnumerable<N> Neighbors { get; }
    }

    public static class AStar
    {
        static public Path<Node> FindPath<Node>(
            Node start,
            Node destination,
            Func<Node, Node, float> distance,
            Func<Node, float> estimate)
            where Node : Neighborable<Node>
        {
            var closed = new HashSet<Node>();
            var queue = new PriorityQueue<float, Path<Node>>();
            queue.Enqueue(0, new Path<Node>(start));
            Path<Node> path = null;
            while (queue.TryDequeue(ref path))
            {
                if (closed.Contains(path.LastStep)) continue;
                if (path.LastStep.Equals(destination)) return path;
                closed.Add(path.LastStep);
                foreach (Node n in path.LastStep.Neighbors)
                {
                    float d = distance(path.LastStep, n);
                    var newPath = path.AddStep(n, d);
                    queue.Enqueue(newPath.TotalCost + estimate(n), newPath);
                }
            }
            return null;
        }
    }
}
