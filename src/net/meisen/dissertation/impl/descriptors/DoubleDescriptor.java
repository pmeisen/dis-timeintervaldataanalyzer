package net.meisen.dissertation.impl.descriptors;

import java.text.DecimalFormat;
import java.text.DecimalFormatSymbols;
import java.util.Locale;

import net.meisen.dissertation.exceptions.DescriptorModelException;
import net.meisen.dissertation.model.datasets.IDataRecord;
import net.meisen.dissertation.model.descriptors.DescriptorModel;
import net.meisen.dissertation.model.descriptors.DescriptorPrimitiveDataType;
import net.meisen.dissertation.model.descriptors.IDescriptorFactory;
import net.meisen.general.genmisc.exceptions.ForwardedRuntimeException;

/**
 * A {@code Descriptor} defined by a {@code Double} value, with at most 5
 * decimal places.
 * 
 * @author pmeisen
 * 
 * @param <I>
 *            the type of the identifier of the index used for the
 *            {@code Descriptor}
 */
public class DoubleDescriptor<I extends Object> extends
		DescriptorPrimitiveDataType<Double, DoubleDescriptor<I>, I> {

	/**
	 * Factory used to ensure the creation of unique identifiers.
	 * 
	 * @author pmeisen
	 * 
	 */
	public static class Factory implements IDescriptorFactory {

		@Override
		public String format(final String value)
				throws ForwardedRuntimeException {
			final double dValue;

			try {
				dValue = Double.parseDouble(value);
			} catch (final NumberFormatException e) {
				throw new ForwardedRuntimeException(
						DescriptorModelException.class, 1011, value);
			}

			return DoubleDescriptor.formatter.format(dValue);
		}
	}

	/**
	 * Formatter used to create the unique string for the double
	 */
	protected final static DecimalFormat formatter = new DecimalFormat(
			"0.#####");

	static {

		// make sure the . and , are used correctly, i.e. US-Format
		final DecimalFormatSymbols symbols = new DecimalFormatSymbols(Locale.US);
		symbols.setDecimalSeparator('.');
		symbols.setGroupingSeparator(',');

		// define the symbols used by the formatter
		formatter.setDecimalFormatSymbols(symbols);
	}

	private double value;

	/**
	 * A constructor which creates a {@code 0.0}-{@code Double} descriptor.
	 * 
	 * @param model
	 *            the {@code DescriptorModel} the {@code Descriptor} belongs to
	 * @param id
	 *            the identifier of the {@code Descriptor}
	 */
	public DoubleDescriptor(final DescriptorModel<I> model, final I id) {
		this(model, id, 0.0);
	}

	/**
	 * A constructor which creates a {@code value}-{@code Double} descriptor.
	 * 
	 * @param model
	 *            the {@code DescriptorModel} the {@code Descriptor} belongs to
	 * @param id
	 *            the identifier of the {@code Descriptor}
	 * @param value
	 *            the integer which specifies the double-value of the descriptor
	 */
	public DoubleDescriptor(final DescriptorModel<I> model, final I id,
			final Integer value) {
		this(model, id, value.doubleValue());
	}

	/**
	 * A constructor which creates a {@code value}-{@code Double} descriptor.
	 * 
	 * @param model
	 *            the {@code DescriptorModel} the {@code Descriptor} belongs to
	 * @param id
	 *            the identifier of the {@code Descriptor}
	 * @param value
	 *            the long which specifies the double-value of the descriptor
	 */
	public DoubleDescriptor(final DescriptorModel<I> model, final I id,
			final Long value) {
		this(model, id, value.doubleValue());
	}

	/**
	 * A constructor which creates a {@code value}-{@code Double} descriptor.
	 * 
	 * @param model
	 *            the {@code DescriptorModel} the {@code Descriptor} belongs to
	 * @param id
	 *            the identifier of the {@code Descriptor}
	 * @param value
	 *            the string which specifies the double-value of the descriptor
	 */
	public DoubleDescriptor(final DescriptorModel<I> model, final I id,
			final String value) {
		this(model, id, Double.parseDouble(value));
	}

	/**
	 * A constructor which creates a {@code value}-{@code Double} descriptor.
	 * 
	 * @param model
	 *            the {@code DescriptorModel} the {@code Descriptor} belongs to
	 * @param id
	 *            the identifier of the {@code Descriptor}
	 * @param value
	 *            the double which specifies the double-value of the descriptor
	 */
	public DoubleDescriptor(final DescriptorModel<I> model, final I id,
			final Double value) {
		super(model, id);
		this.value = value;
	}

	/**
	 * A constructor which creates a {@code value}-{@code Double} descriptor.
	 * 
	 * @param model
	 *            the {@code DescriptorModel} the {@code Descriptor} belongs to
	 * @param id
	 *            the identifier of the {@code Descriptor}
	 * @param value
	 *            the number which specifies the double-value of the descriptor
	 */
	public DoubleDescriptor(final DescriptorModel<I> model, final I id,
			final Number value) {
		super(model, id);
		this.value = mapToDataType(value);
	}

	@Override
	public Double getValue() {
		return value;
	}

	@Override
	public DoubleDescriptor<I> clone() {
		return new DoubleDescriptor<I>(getModel(), getId(), value);
	}

	@Override
	public Class<Double> getPrimitiveDataType() {
		return Double.class;
	}

	@Override
	public boolean valueEquals(final DoubleDescriptor<I> descriptor) {
		return descriptor.value == value;
	}

	@Override
	public String getUniqueString() {
		return formatter.format(value);
	}

	@Override
	public double getFactValue(final IDataRecord record) {
		return value;
	}

	@Override
	public boolean isRecordInvariant() {
		return true;
	}

	@Override
	public boolean isValueInvariant() {
		return false;
	}
}
