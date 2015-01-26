package net.meisen.dissertation.impl.parser.query.select.logical;

import java.util.ArrayDeque;
import java.util.ArrayList;
import java.util.Deque;
import java.util.List;

import net.meisen.dissertation.impl.parser.query.select.IComperator;
import net.meisen.dissertation.model.descriptors.Descriptor;

/**
 * Logic representing a filter definition, based on values of {@code Descriptor}
 * instances.
 * 
 * @author pmeisen
 * 
 * @see Descriptor
 * 
 */
public class DescriptorLogicTree {
	private final RootNode root;

	private LogicalOperatorNode currentNode;

	/**
	 * Constructor to create an empty logic-tree.
	 */
	public DescriptorLogicTree() {
		this.root = new RootNode();
		this.currentNode = root;
	}
	
	/**
	 * Gets the root of the tree.
	 * 
	 * @return the root of the tree
	 */
	public LogicalOperatorNode getRoot() {
		return root;
	}

	/**
	 * Attaches the specified {@code LogicalOperator} to the tree. The
	 * {@code LogicalOperator} is attached as {@code LogicalOperatorNode} to the
	 * current node. The <b>current node is changed</b> to be the attached node.
	 * 
	 * @param operator
	 *            the {@code LogicalOperator} to be attached
	 */
	public void attach(final LogicalOperator operator) {

		// add a new logicNode
		final LogicalOperatorNode ln = new LogicalOperatorNode(
				this.currentNode, operator);
		this.currentNode.attachChild(ln);
		this.currentNode = ln;
	}

	/**
	 * Attaches the specified {@code DescriptorComperator} to the tree. The
	 * {@code DescriptorComperator} is attached as is to the current node. The
	 * <b>current node is not changed</b> to be the attached node.
	 * 
	 * @param dc
	 *            the {@code DescriptorComperator} to be attached
	 */
	public void attach(final IComperator dc) {
		this.currentNode.attachChild(dc);
	}

	/**
	 * Moves the current node up to the parent of the current node. If the
	 * current node is the root node, an exception is thrown.
	 * 
	 * @throws IllegalStateException
	 *             if the method is called and the current node is the root node
	 */
	public void moveUp() throws IllegalStateException {
		final LogicalOperatorNode parent = this.currentNode.getParent();
		if (parent == null) {
			throw new IllegalStateException(
					"Cannot move up anymore, the root is reached");
		} else {
			this.currentNode = parent;
		}
	}

	/**
	 * Creates a list of elements of the tree ordered by evaluation order of the
	 * different nodes of the tree.
	 * 
	 * @return a list of elements of the tree ordered by evaluation order
	 */
	public List<ILogicalTreeElement> getEvaluationOrder() {
		final List<ILogicalTreeElement> order = new ArrayList<ILogicalTreeElement>();

		// check if the root has children, otherwise there is nothing to do
		if (root.getChildren().size() != 0) {

			final Deque<ILogicalTreeElement> stack = new ArrayDeque<ILogicalTreeElement>();
			fillStack(root, stack);

			while (!stack.isEmpty()) {
				order.add(stack.pop());
			}
		}

		return order;
	}

	/**
	 * Helper method to create the evaluation ordered list.
	 * 
	 * @param node
	 *            the current node used to fill the stack
	 * @param stack
	 *            the stack to be filled
	 */
	protected void fillStack(final LogicalOperatorNode node,
			final Deque<ILogicalTreeElement> stack) {

		final List<ILogicalTreeElement> children = node.getChildren();
		for (int i = children.size(); i > 0; i--) {
			final ILogicalTreeElement child = children.get(i - 1);

			stack.push(child);
			if (child instanceof LogicalOperatorNode) {
				fillStack((LogicalOperatorNode) child, stack);
			}
		}
	}

	@Override
	public String toString() {
		return root.toString();
	}

	/**
	 * Optimizes the whole tree, so that logical expressions are combined if
	 * possible.
	 */
	public void optimize() {

		// merge equal logical expressions
		mergeLogicalOperators(root);
	}

	/**
	 * Merges the {@code current} node and it's parent if logically possible.
	 * 
	 * @param current
	 *            the node to be checked for merging
	 */
	protected void mergeLogicalOperators(final LogicalOperatorNode current) {

		// get the parent
		final LogicalOperatorNode currentParent = current.getParent();
		final LogicalOperator currentParentOp = currentParent == null ? null
				: currentParent.get();

		// get the children
		final List<ILogicalTreeElement> currentChildren = current.getChildren();

		/*
		 * Check if there is a parent and if so, check if the parent's
		 * LogicalOperator is equal to the current's LogicalOperator. The
		 * current node is not needed and all children can be attached to the
		 * parent if so.
		 */
		if (currentParentOp != null && currentParentOp.equals(current.get())) {
			for (int i = currentChildren.size(); i > 0; i--) {
				final ILogicalTreeElement currentChild = currentChildren
						.get(i - 1);
				currentParent.attachChildFirst(currentChild);
				currentChild.setParent(currentParent);
			}
			currentParent.removeChild(current);
		}

		// now go for all the children and check if those can be changed
		for (final ILogicalTreeElement currentChild : currentChildren) {

			// if the child is a LogicalOperatorNode it might be optimizable
			if (currentChild instanceof LogicalOperatorNode) {
				final LogicalOperatorNode childNode = (LogicalOperatorNode) currentChild;
				mergeLogicalOperators(childNode);
			}
		}
	}
}
