package net.meisen.dissertation.impl.parser.query.select;

import net.meisen.dissertation.impl.parser.query.DimensionSelector;

/**
 * A comparator used for {@code Dimensions}.
 * 
 * @author pmeisen
 * 
 */
public class DimensionComperator extends DescriptorValue implements IComperator {
	private DimensionSelector dim;

	/**
	 * Creates a {@code DimensionComperator} for the dimensions specified by the
	 * {@code dim}. The value of the member of the level is compared to the
	 * specified value using the {@link #matches(String)} method.
	 * 
	 * @param dim
	 *            the identifier of the {@code Dimension}
	 * @param value
	 *            the actual defined value or a regular expression if a
	 *            wildchars was contained in the comparison-value set by
	 *            {@link #setValue(String)}
	 */
	public DimensionComperator(final DimensionSelector dim, final String value) {
		super(value);

		setDimension(dim);
		setValue(value);
	}

	@Override
	public String toString() {
		return getDimension().toString() + " = " + getRawValue();
	}

	/**
	 * Gets the {@code DimensionSelector} of {@code this}.
	 * 
	 * @return the {@code DimensionSelector} of {@code this}
	 */
	public DimensionSelector getDimension() {
		return dim;
	}

	/**
	 * Sets the {@code DimensionSelector} of {@code this}.
	 * 
	 * @param dim
	 *            the {@code DimensionSelector} to be used
	 */
	public void setDimension(final DimensionSelector dim) {
		this.dim = dim;
	}
}
