package net.meisen.dissertation.impl.parser.query.select.logical;

import java.util.ArrayList;
import java.util.List;

import net.meisen.dissertation.impl.parser.query.select.DescriptorComperator;
import net.meisen.general.genmisc.types.Strings;

/**
 * A node representing a {@code LogicalOperator} within a
 * {@code DescriptorLogicTree}.
 * 
 * @author pmeisen
 * 
 */
public class LogicalOperatorNode implements ITreeElement {

	private final List<ITreeElement> children;
	private final LogicalOperator operator;

	private LogicalOperatorNode parent;

	/**
	 * Default constructor only be used internally.
	 */
	protected LogicalOperatorNode() {
		this.children = new ArrayList<ITreeElement>();
		this.parent = null;
		this.operator = null;
	}

	/**
	 * Creates a node with the specified parent.
	 * 
	 * @param parent
	 *            the parent node of {@code this}
	 */
	public LogicalOperatorNode(final LogicalOperatorNode parent) {
		this(parent, null);
	}

	/**
	 * Creates a node with the specified parent, which represents the specified
	 * {@code LogicalOperator}.
	 * 
	 * @param parent
	 *            the parent node of {@code this}
	 * @param operator
	 *            the {@code LogicalOperator} represented by the node
	 * 
	 * @see LogicalOperator
	 */
	public LogicalOperatorNode(final LogicalOperatorNode parent,
			final LogicalOperator operator) {
		if (parent == null) {
			throw new NullPointerException("The parent cannot be null.");
		}

		this.operator = operator;
		this.children = new ArrayList<ITreeElement>();
		this.parent = parent;
	}

	/**
	 * Gets the {@code LogicalOperator} represented by the node.
	 * 
	 * @return the {@code LogicalOperator} represented by the node
	 */
	public LogicalOperator get() {
		return this.operator;
	}

	@Override
	public LogicalOperatorNode getParent() {
		return this.parent;
	}

	/**
	 * Get the i-th child of the node.
	 * 
	 * @param i
	 *            the number of the child to be retrieved
	 * @return the i-th child
	 * 
	 * @throws IndexOutOfBoundsException
	 *             if the i-th child doesn't exist
	 */
	public ITreeElement getChild(final int i) throws IndexOutOfBoundsException {
		return children.get(i);
	}

	/**
	 * Gets all the children of the node.
	 * 
	 * @return all the children of the node
	 */
	public List<ITreeElement> getChildren() {
		return new ArrayList<ITreeElement>(children);
	}

	/**
	 * Attaches a {@code DescriptorComperator} to the end of all the children of
	 * {@code this}.
	 * 
	 * @param dc
	 *            the {@code DescriptorComperator} to be attached
	 */
	public void attachChild(final DescriptorComperator dc) {
		attachChild(new DescriptorLeaf(this, dc));
	}

	/**
	 * Attaches a child to the end of all the children of {@code this}.
	 * 
	 * @param treeElement
	 *            the child to be attached
	 */
	public void attachChild(final ITreeElement treeElement) {
		this.children.add(treeElement);
	}

	/**
	 * Attaches a child to the start of all the children of {@code this}.
	 * 
	 * @param treeElement
	 *            the child to be attached
	 */
	public void attachChildFirst(final ITreeElement treeElement) {
		this.children.add(0, treeElement);
	}

	/**
	 * Removes a child from the node.
	 * 
	 * @param node
	 *            the node to be removed
	 */
	public void removeChild(final LogicalOperatorNode node) {
		this.children.remove(node);
	}

	@Override
	public String toString() {
		return operator + "("
				+ Strings.smartTrimSequence(this.children.toString(), "[")
				+ ")";
	}

	@Override
	public void setParent(final LogicalOperatorNode parent) {
		if (this.parent == null) {
			throw new IllegalStateException(
					"The parent of the root cannot be changed.");
		}
		this.parent = parent;
	}
}
