package net.meisen.dissertation.impl.parser.query.select.logical;

import net.meisen.dissertation.impl.parser.query.select.DescriptorComperator;

public class DescriptorLeaf implements ITreeElement {

	private final DescriptorComperator descriptorComparator;

	private LogicalOperatorNode parent;
	
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

	@Override
	public void setParent(final LogicalOperatorNode parent) {
		this.parent = parent;		
	}
}
