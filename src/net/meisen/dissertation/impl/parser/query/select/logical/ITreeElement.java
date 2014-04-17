package net.meisen.dissertation.impl.parser.query.select.logical;

/**
 * An element of a tree, i.e. something that has exactly one parent.
 * 
 * @author pmeisen
 * 
 */
public interface ITreeElement {

	/**
	 * Gets the parent of the {@code TreeElement}.
	 * 
	 * @return the parent of the {@code TreeElement}
	 */
	public ITreeElement getParent();

	/**
	 * Sets the parent of the {@code TreeElement}.
	 * 
	 * @param parent
	 *            the parent to be
	 */
	public void setParent(final LogicalOperatorNode parent);
}
