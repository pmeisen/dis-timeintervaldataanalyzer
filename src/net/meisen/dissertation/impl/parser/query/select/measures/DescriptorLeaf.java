package net.meisen.dissertation.impl.parser.query.select.measures;


/**
 * A leaf of a tree which is used to represent a {@code Descriptor} by it's
 * identifier.
 * 
 * @author pmeisen
 * 
 */
public class DescriptorLeaf implements IMathTreeElement {

	private final String id;

	private MathOperatorNode parent;

	/**
	 * Constructor to create a leaf for the specified {@code parent} and using
	 * the specified {@code id}.
	 * 
	 * @param parent
	 *            the parent
	 * @param id
	 *            the identifier specifying the {@code Descriptor}
	 */
	public DescriptorLeaf(final MathOperatorNode parent, final String id) {
		this.parent = parent;
		this.id = id;
	}

	/**
	 * Gets the identifier defined for the leaf.
	 * 
	 * @return the {@code identifier} defined for the leaf
	 */
	public String get() {
		return id;
	}

	@Override
	public String toString() {
		return id;
	}

	@Override
	public MathOperatorNode getParent() {
		return parent;
	}

	@Override
	public void setParent(final MathOperatorNode parent) {
		this.parent = parent;
	}
	
	@Override
	public int amountOfChildren() {
		return 0;
	}
}
