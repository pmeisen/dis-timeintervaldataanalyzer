package net.meisen.dissertation.model.descriptors;

import net.meisen.dissertation.model.datasets.IDataRecord;

/***
 * A special {@code Descriptor} which is used to support {@code null} values
 * within a {@code DescriptorModel}.
 * 
 * @author pmeisen
 * 
 * @param <I>
 *            the identifier used to identify the {@code Descriptor}
 */
public class NullDescriptor<I extends Object> extends
		Descriptor<Object, NullDescriptor<I>, I> {
	/**
	 * The name used when none is defined
	 */
	public static final String DEF_NAME = "{null}";

	private final String name;

	/**
	 * Creates a {@code NullDescriptor} for the specified {@code model} with the
	 * specified {@code id}.
	 * 
	 * @param model
	 *            the {@code DescriptorModel} the {@code Descriptor} belongs to
	 * @param id
	 *            the identifier of the {@code NullDescriptor}
	 */
	public NullDescriptor(final DescriptorModel<I> model, final I id) {
		this(null, model, id);
	}

	/**
	 * Creates a {@code NullDescriptor} for the specified {@code model} with the
	 * specified {@code id}.
	 * 
	 * @param model
	 *            the {@code DescriptorModel} the {@code Descriptor} belongs to
	 * @param id
	 *            the identifier of the {@code NullDescriptor}
	 * @param name
	 *            a name for the {@code Descriptor}, by default
	 *            {@link #DEF_NAME} is used
	 */
	public NullDescriptor(final String name, final DescriptorModel<I> model,
			final I id) {
		super(model, id);
		this.name = name == null ? DEF_NAME : name;
	}

	@Override
	public Object getValue() {
		return null;
	}

	@Override
	public NullDescriptor<I> clone() {
		return this;
	}

	@Override
	public String toString() {
		return name;
	}

	@Override
	public String getUniqueString() {
		return null;
	}

	@Override
	public double getFactValue(final IDataRecord record) {
		return 0.0;
	}

	@Override
	public boolean isRecordInvariant() {
		return true;
	}

	@Override
	public boolean isValueInvariant() {
		return true;
	}
}
