package net.meisen.dissertation.impl.parser.query.select.measures;

import java.util.ArrayList;
import java.util.List;

import net.meisen.general.genmisc.types.Strings;

/**
 * A node representing a mathematical expression, i.e. a {@code MathOperator}.
 * 
 * @author pmeisen
 * 
 * @see MathOperator
 * 
 */
public class MathOperatorNode implements IMathTreeElement {

	private final List<IMathTreeElement> children;
	private final MathOperator operator;

	private MathOperatorNode parent;

	/**
	 * Default constructor only be used internally.
	 */
	protected MathOperatorNode() {
		this.children = new ArrayList<IMathTreeElement>();
		this.parent = null;
		this.operator = null;
	}

	/**
	 * Creates a node with the specified parent.
	 * 
	 * @param parent
	 *            the parent node of {@code this}
	 */
	public MathOperatorNode(final MathOperatorNode parent) {
		this(parent, null);
	}

	/**
	 * Creates a node with the specified parent, which represents the specified
	 * {@code MathOperator}.
	 * 
	 * @param parent
	 *            the parent node of {@code this}
	 * @param operator
	 *            the {@code MathOperator} represented by the node
	 * 
	 * @see ArithmeticOperator
	 */
	public MathOperatorNode(final MathOperatorNode parent,
			final MathOperator operator) {
		if (parent == null) {
			throw new NullPointerException("The parent cannot be null.");
		}

		this.operator = operator;
		this.children = new ArrayList<IMathTreeElement>();
		this.parent = parent;
	}

	/**
	 * Gets the {@code MathOperator} represented by the node.
	 * 
	 * @return the {@code MathOperator} represented by the node
	 */
	public MathOperator get() {
		return this.operator;
	}

	@Override
	public MathOperatorNode getParent() {
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
	public IMathTreeElement getChild(final int i)
			throws IndexOutOfBoundsException {
		return children.get(i);
	}

	/**
	 * Gets all the children of the node.
	 * 
	 * @return all the children of the node
	 */
	public List<IMathTreeElement> getChildren() {
		return new ArrayList<IMathTreeElement>(children);
	}

	/**
	 * Attaches a {@code Descriptor} to the end of all the children of
	 * {@code this}.
	 * 
	 * @param descriptorId
	 *            the identifier of the {@code Descriptor} to be attached
	 */
	public void attachChild(final String descriptorId) {
		attachChild(new DescriptorLeaf(this, descriptorId));
	}

	/**
	 * Attaches a child to the end of all the children of {@code this}.
	 * 
	 * @param mathTreeElement
	 *            the child to be attached
	 */
	public void attachChild(final IMathTreeElement mathTreeElement) {
		this.children.add(mathTreeElement);
	}

	/**
	 * Attaches a child to the start of all the children of {@code this}.
	 * 
	 * @param mathTreeElement
	 *            the child to be attached
	 */
	public void attachChildFirst(final IMathTreeElement mathTreeElement) {
		this.children.add(0, mathTreeElement);
	}

	/**
	 * Removes a child from the node.
	 * 
	 * @param node
	 *            the node to be removed
	 */
	public void removeChild(final MathOperatorNode node) {
		this.children.remove(node);
	}

	@Override
	public String toString() {
		return operator + "("
				+ Strings.smartTrimSequence(this.children.toString(), "[")
				+ ")";
	}

	@Override
	public void setParent(final MathOperatorNode parent) {
		if (this.parent == null) {
			throw new IllegalStateException(
					"The parent of the root cannot be changed.");
		}
		this.parent = parent;
	}

	@Override
	public int amountOfChildren() {
		return children.size();
	}
}
