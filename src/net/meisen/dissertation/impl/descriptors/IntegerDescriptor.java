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
 * A {@code Descriptor} defined by a {@code Integer} value.
 * 
 * @author pmeisen
 * 
 * @param <I>
 *            the type of the identifier of the index used for the
 *            {@code Descriptor}
 */
public class IntegerDescriptor<I extends Object> extends
		DescriptorPrimitiveDataType<Integer, IntegerDescriptor<I>, I> {

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
			final int iValue;

			try {
				iValue = Integer.parseInt(value);
			} catch (final NumberFormatException e) {
				throw new ForwardedRuntimeException(
						DescriptorModelException.class, 1011, value);
			}

			return IntegerDescriptor.formatter.format(iValue);
		}
	}

	/**
	 * Formatter used to create the unique string for the double
	 */
	protected final static DecimalFormat formatter = new DecimalFormat("0");

	static {

		// make sure the . and , are used correctly, i.e. US-Format
		final DecimalFormatSymbols symbols = new DecimalFormatSymbols(Locale.US);
		symbols.setDecimalSeparator('.');
		symbols.setGroupingSeparator(',');

		// define the symbols used by the formatter
		formatter.setDecimalFormatSymbols(symbols);
	}

	private int value;

	/**
	 * A constructor which creates a {@code 0}-{@code Integer} descriptor.
	 * 
	 * @param model
	 *            the {@code DescriptorModel} the {@code Descriptor} belongs to
	 * @param id
	 *            the identifier of the {@code Descriptor}
	 */
	public IntegerDescriptor(final DescriptorModel<I> model, final I id) {
		this(model, id, 0);
	}

	/**
	 * A constructor which creates a {@code value}-{@code Integer} descriptor.
	 * 
	 * @param model
	 *            the {@code DescriptorModel} the {@code Descriptor} belongs to
	 * @param id
	 *            the identifier of the {@code Descriptor}
	 * @param value
	 *            the string which specifies the integer-value of the descriptor
	 */
	public IntegerDescriptor(final DescriptorModel<I> model, final I id,
			final String value) {
		this(model, id, Integer.parseInt(value));
	}

	/**
	 * A constructor which creates a {@code value}-{@code Integer} descriptor.
	 * 
	 * @param model
	 *            the {@code DescriptorModel} the {@code Descriptor} belongs to
	 * @param id
	 *            the identifier of the {@code Descriptor}
	 * @param value
	 *            the integer which specifies the integer-value of the
	 *            descriptor
	 */
	public IntegerDescriptor(final DescriptorModel<I> model, final I id,
			final Integer value) {
		super(model, id);
		this.value = value;
	}

	/**
	 * A constructor which creates a {@code value}-{@code Integer} descriptor.
	 * 
	 * @param model
	 *            the {@code DescriptorModel} the {@code Descriptor} belongs to
	 * @param id
	 *            the identifier of the {@code Descriptor}
	 * @param value
	 *            the number which specifies the integer-value of the descriptor
	 */
	public IntegerDescriptor(final DescriptorModel<I> model, final I id,
			final Number value) {
		super(model, id);
		this.value = mapToDataType(value);
	}

	@Override
	public Integer getValue() {
		return value;
	}

	@Override
	public IntegerDescriptor<I> clone() {
		return new IntegerDescriptor<I>(getModel(), getId(), value);
	}

	@Override
	public Class<Integer> getPrimitiveDataType() {
		return Integer.class;
	}

	@Override
	public boolean valueEquals(final IntegerDescriptor<I> descriptor) {
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
