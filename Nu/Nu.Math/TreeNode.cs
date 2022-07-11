using System;
using System.Collections.Generic;
using System.Collections.ObjectModel;
using System.Linq;

namespace Nu
{
    /// <summary>
    /// A generic tree node.
    /// Sourced from - https://stackoverflow.com/a/10442244/1082782
    /// </summary>
    public class TreeNode<T>
    {
        public TreeNode<T> Parent { get; private set; }

        public T Value { get { return value; } }

        public ReadOnlyCollection<TreeNode<T>> Children
        {
            get { return children.AsReadOnly(); }
        }

        public TreeNode(T value)
        {
            this.value = value;
        }

        public void Add(TreeNode<T> node)
        {
            children.Add(node);
        }

        public void AddMany(IEnumerable<TreeNode<T>> children)
        {
            foreach (var child in children)
                Add(child);
        }

        public bool Remove(TreeNode<T> node)
        {
            return children.Remove(node);
        }

        public void Traverse(Action<T> action)
        {
            action(Value);
            foreach (var child in children)
                child.Traverse(action);
        }

        public IEnumerable<T> Flatten()
        {
            return new[] { Value }.Concat(children.SelectMany(x => x.Flatten()));
        }

        private readonly T value;
        private readonly List<TreeNode<T>> children = new List<TreeNode<T>>();
    }
}
