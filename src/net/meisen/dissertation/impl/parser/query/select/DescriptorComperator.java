package net.meisen.dissertation.impl.parser.query.select;


/**
 * A comparator used for {@code Descriptors}.
 * 
 * @author pmeisen
 * 
 */
public class DescriptorComperator extends DescriptorValue implements IComperator {
	private String id;

	/**
	 * Creates a {@code DescriptorComperator} for the descriptor specified by
	 * the {@code id}. The value of the descriptor is compared to the specified
	 * value using the {@link #matches(String)} method.
	 * 
	 * @param id
	 *            the identifier of the descriptor
	 * @param value
	 *            the actual defined value or a regular expression if a
	 *            wildchars was contained in the comparison-value set by
	 *            {@link #setValue(String)}
	 */
	public DescriptorComperator(final String id, final String value) {
		super(value);

		setId(id);
		setValue(value);
	}

	/**
	 * Gets the identifier of the descriptor.
	 * 
	 * @return the identifier of the descriptor
	 */
	public String getId() {
		return id;
	}

	/**
	 * Sets the identifier of the descriptor to be used for comparison.
	 * 
	 * @param id
	 *            the identifier of the descriptor
	 */
	public void setId(final String id) {
		this.id = id;
	}

	@Override
	public String toString() {
		return id + " = " + getRawValue();
	}
}
