package net.meisen.dissertation.impl.parser.query.select.logical;

import net.meisen.dissertation.impl.parser.query.select.DescriptorComperator;

/**
 * A leaf of a tree which is used to represent a {@code Descriptor} using a
 * {@code DescriptorComperator}.
 * 
 * @author pmeisen
 * 
 * @see DescriptorComperator
 * 
 */
public class DescriptorLeaf implements ILogicalTreeElement {

	private final DescriptorComperator descriptorComparator;

	private LogicalOperatorNode parent;

	/**
	 * Constructor to create a leaf for the specified {@code parent} and using
	 * the specified {@code dc}.
	 * 
	 * @param parent
	 *            the parent
	 * @param dc
	 *            the {@code DescriptorComperator} specifying the
	 *            {@code Descriptor} and the comparator
	 */
	public DescriptorLeaf(final LogicalOperatorNode parent,
			final DescriptorComperator dc) {
		this.parent = parent;
		this.descriptorComparator = dc;
	}

	/**
	 * Gets the {@code DescriptorComperator} defined for the leaf.
	 * 
	 * @return the {@code DescriptorComperator} defined for the leaf
	 */
	public DescriptorComperator get() {
		return descriptorComparator;
	}

	@Override
	public String toString() {
		return descriptorComparator.toString();
	}

	@Override
	public LogicalOperatorNode getParent() {
		return parent;
	}

	@Override
	public void setParent(final LogicalOperatorNode parent) {
		this.parent = parent;
	}
}
