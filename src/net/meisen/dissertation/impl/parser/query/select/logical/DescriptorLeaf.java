package net.meisen.dissertation.impl.parser.query.select.logical;

import net.meisen.dissertation.impl.parser.query.select.IComperator;
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

	private final IComperator comparator;

	private LogicalOperatorNode parent;

	/**
	 * Constructor to create a leaf for the specified {@code parent} and using
	 * the specified {@code dc}.
	 * 
	 * @param parent
	 *            the parent
	 * @param dc
	 *            the {@code IComperator} specifying the
	 *            {@code Descriptor} and the comparator
	 */
	public DescriptorLeaf(final LogicalOperatorNode parent,
			final IComperator dc) {
		this.parent = parent;
		this.comparator = dc;
	}

	/**
	 * Gets the {@code IComperator} defined for the leaf.
	 * 
	 * @return the {@code IComperator} defined for the leaf
	 */
	public IComperator get() {
		return comparator;
	}

	@Override
	public String toString() {
		return comparator.toString();
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
