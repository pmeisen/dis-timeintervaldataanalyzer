package net.meisen.dissertation.parser.query.select.logical;

import net.meisen.dissertation.parser.query.select.DescriptorComperator;

public class DescriptorLeaf implements ITreeElement {

	private final LogicalOperatorNode parent;
	private final DescriptorComperator descriptorComparator;

	public DescriptorLeaf(final LogicalOperatorNode parent,
			final DescriptorComperator dc) {
		this.parent = parent;
		this.descriptorComparator = dc;
	}

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
}
