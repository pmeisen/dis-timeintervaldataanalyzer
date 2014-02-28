package net.meisen.dissertation.parser.query.select.logical;

import java.util.ArrayDeque;
import java.util.ArrayList;
import java.util.Deque;
import java.util.List;

import net.meisen.dissertation.parser.query.select.DescriptorComperator;

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
}
