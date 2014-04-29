package net.meisen.dissertation.impl.parser.query.select.measures;

/**
 * Element of a tree representing a math-expression.
 * 
 * @author pmeisen
 * 
 */
public interface IMathTreeElement {

	/**
	 * Gets the parent of the {@code TreeElement}.
	 * 
	 * @return the parent of the {@code TreeElement}
	 */
	public IMathTreeElement getParent();

	/**
	 * Sets the parent of the {@code TreeElement}.
	 * 
	 * @param parent
	 *            the parent to be
	 */
	public void setParent(final MathOperatorNode parent);

	/**
	 * Gets the amount of children of the element.
	 * 
	 * @return the amount of children of the element
	 */
	public int amountOfChildren();
}
