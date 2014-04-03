package net.meisen.dissertation.impl.parser.query.select.logical;

import java.util.ArrayDeque;
import java.util.ArrayList;
import java.util.Deque;
import java.util.List;

import net.meisen.dissertation.impl.parser.query.select.DescriptorComperator;

public class DescriptorLogicTree {
	private RootNode root;
	private LogicalOperatorNode currentNode;

	public DescriptorLogicTree() {
		this.root = new RootNode();
		this.currentNode = root;
	}

	public void attach(final LogicalOperator operator) {

		// add a new logicNode
		final LogicalOperatorNode ln = new LogicalOperatorNode(
				this.currentNode, operator);
		this.currentNode.attachChild(ln);
		this.currentNode = ln;
	}

	public void attach(final DescriptorComperator dc) {
		this.currentNode.attachChild(dc);
	}

	public void moveUp() {
		final LogicalOperatorNode parent = this.currentNode.getParent();
		if (parent == null) {
			throw new IllegalStateException(
					"Cannot move up anymore, the root is reached");
		} else {
			this.currentNode = parent;
		}
	}

	public List<ITreeElement> getEvaluationOrder() {
		final List<ITreeElement> order = new ArrayList<ITreeElement>();

		// check if there even might be an order
		if (root.getChildren().size() != 0) {

			final Deque<ITreeElement> stack = new ArrayDeque<ITreeElement>();
			fillStack(root, stack);

			while (!stack.isEmpty()) {
				order.add(stack.pop());
			}
		}

		return order;
	}

	@Override
	public String toString() {
		return root.toString();
	}

	protected void fillStack(final LogicalOperatorNode node,
			final Deque<ITreeElement> stack) {

		final List<ITreeElement> children = node.getChildren();
		for (int i = children.size(); i > 0; i--) {
			final ITreeElement child = children.get(i - 1);

			stack.push(child);
			if (child instanceof LogicalOperatorNode) {
				fillStack((LogicalOperatorNode) child, stack);
			}
		}
	}

	public void optimize() {

		// merge equal logical expressions
		mergeLogicalOperators(root);
	}

	protected void mergeLogicalOperators(final LogicalOperatorNode current) {

		// get the parent
		final LogicalOperatorNode currentParent = current.getParent();
		final LogicalOperator currentParentOp = currentParent == null ? null
				: currentParent.get();

		// get the children
		final List<ITreeElement> currentChildren = current.getChildren();

		/*
		 * Check if there is a parent and if so, check if the parent's
		 * LogicalOperator is equal to the current's LogicalOperator. The
		 * current node is not needed and all children can be attached to the
		 * parent if so.
		 */
		if (currentParentOp != null && currentParentOp.equals(current.get())) {
			for (int i = currentChildren.size(); i > 0; i--) {
				final ITreeElement currentChild = currentChildren.get(i - 1);
				currentParent.attachChildFirst(currentChild);
				currentChild.setParent(currentParent);
			}
			currentParent.removeChild(current);
		}

		// now go for all the children and check if those can be changed
		for (final ITreeElement currentChild : currentChildren) {

			// if the child is a LogicalOperatorNode it might be optimizable
			if (currentChild instanceof LogicalOperatorNode) {
				final LogicalOperatorNode childNode = (LogicalOperatorNode) currentChild;
				mergeLogicalOperators(childNode);
			}
		}
	}
}
