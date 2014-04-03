package net.meisen.dissertation.impl.parser.query.select.logical;

public interface ITreeElement {

	public ITreeElement getParent();
	
	public void setParent(final LogicalOperatorNode parent);
}
