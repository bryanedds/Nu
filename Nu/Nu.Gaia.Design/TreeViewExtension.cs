using System;
using System.Collections.Generic;
using System.Linq;

namespace System.Windows.Forms
{
    /// <summary>
    /// TreeView extension type.
    /// Code taken from https://stackoverflow.com/a/54349869/1082782.
    /// </summary>
    public static class TreeViewExtensions
    {
        public static TreeViewState GetExpandedNodesState(this TreeView tree)
        {
            var expandedNodesList = new List<string>();
            foreach (TreeNode node in tree.Nodes)
                UpdateExpandedList(ref expandedNodesList, node);
            return new TreeViewState(expandedNodesList, tree.TopNode, tree.SelectedNode);
        }

        public static void RestoreExpandedNodesState(this TreeView tree, TreeViewState state)
        {
            tree.BeginUpdate();
            {
                foreach (TreeNode node in tree.Nodes)
                    foreach (var nodeState in state.ExpandedNodes)
                        ExpandNodes(node, nodeState);

                tree.TopNode = TryGetNodeFromPath(tree, state.TopNodePath);
                tree.SelectedNode = TryGetNodeFromPath(tree, state.SelectedNodePath);
            }
            tree.EndUpdate();
        }

        public static TreeNode TryGetNodeFromPath(this TreeView tree, string path)
        {
            if (string.IsNullOrWhiteSpace(path))
                return null;

            List<string> elements = path.Split(tree.PathSeparator.ToCharArray()).ToList();

            TreeNode curNode = tree.Nodes.findByText(elements[0]);
            if (curNode == null)
                return null;

            foreach (string element in elements.Skip(1))
            {
                if (curNode.Nodes.findByText(element) != null)
                    curNode = curNode.Nodes.findByText(element);
                else
                    break;
            }

            return curNode;
        }

        static TreeNode findByText(this TreeNodeCollection tnc, string text)
        {
            foreach (TreeNode node in tnc)
                if (node.Text == text)
                    return node;
            return null;
        }

        static void UpdateExpandedList(ref List<string> expNodeList, TreeNode node)
        {
            if (node.IsExpanded) expNodeList.Add(node.FullPath);
            foreach (TreeNode n in node.Nodes)
                if (n.IsExpanded)
                    UpdateExpandedList(ref expNodeList, n);
        }

        static void ExpandNodes(TreeNode node, string nodeFullPath)
        {
            if (node.FullPath == nodeFullPath) node.Expand();
            foreach (TreeNode n in node.Nodes)
                if (n.Nodes.Count > 0)
                    ExpandNodes(n, nodeFullPath);
        }
    }

    public class TreeViewState
    {
        public TreeViewState(List<string> expandedNodes, TreeNode topNode, TreeNode selectedNode)
        {
            ExpandedNodes = expandedNodes;
            TopNodePath = topNode != null ? topNode.FullPath : null;
            SelectedNodePath = selectedNode != null ? selectedNode.FullPath : null;
        }

        public readonly List<string> ExpandedNodes = null;
        public readonly string TopNodePath = "";
        public readonly string SelectedNodePath = "";
    }
}
