package net.meisen.dissertation.impl.descriptors;

import java.util.Arrays;
import java.util.Collection;
import java.util.List;

import net.meisen.dissertation.model.datasets.IDataRecord;
import net.meisen.dissertation.model.descriptors.Descriptor;
import net.meisen.dissertation.model.descriptors.DescriptorModel;
import net.meisen.dissertation.model.descriptors.IDescriptorFactory;
import net.meisen.general.genmisc.exceptions.ForwardedRuntimeException;
import net.meisen.general.genmisc.types.Strings;

/**
 * Simple {@code Descriptor} to handle {@code List} instances.
 * 
 * @author pmeisen
 * 
 * @param <I>
 *            the type of the indizes used
 */
public class ListDescriptor<I extends Object> extends
		Descriptor<String[], ListDescriptor<I>, I> {
	private final static String SEPARATOR = ",";

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
			final String[] values = value.split(",");
			Arrays.sort(values);

			return Strings.join(SEPARATOR, values);
		}
	}

	private String[] values;

	/**
	 * Constructor without any value assignment.
	 * 
	 * @param model
	 *            the {@code DescriptorModel} of the {@code Descriptor}
	 * @param id
	 *            the identifier of the {@code Descriptor}
	 */
	public ListDescriptor(final DescriptorModel<I> model, final I id) {
		this(model, id, (String) null);
	}

	/**
	 * Constructor with {@code String}-value assignment.
	 * 
	 * @param model
	 *            the {@code DescriptorModel} of the {@code Descriptor}
	 * @param id
	 *            the identifier of the {@code Descriptor}
	 * @param value
	 *            the value to be assigned
	 */
	public ListDescriptor(final DescriptorModel<I> model, final I id,
			final String value) {
		this(model, id, value == null ? new String[] {} : value
				.split(SEPARATOR));
	}

	/**
	 * Constructor with {@code Collection}-value assignment.
	 * 
	 * @param model
	 *            the {@code DescriptorModel} of the {@code Descriptor}
	 * @param id
	 *            the identifier of the {@code Descriptor}
	 * @param value
	 *            the value to be assigned
	 */
	public ListDescriptor(final DescriptorModel<I> model, final I id,
			final Collection<String> value) {
		this(model, id, value == null ? new String[] {} : value
				.toArray(new String[] {}));
	}

	/**
	 * Constructor with {@code String-Array}-value assignment.
	 * 
	 * @param model
	 *            the {@code DescriptorModel} of the {@code Descriptor}
	 * @param id
	 *            the identifier of the {@code Descriptor}
	 * @param value
	 *            the value to be assigned
	 */
	public ListDescriptor(final DescriptorModel<I> model, final I id,
			final String[] value) {
		super(model, id);

		Arrays.sort(value);
		this.values = value;
	}

	/**
	 * Gets the assigned {@code List} of values.
	 * 
	 * @return the assigned {@code List} of values
	 */
	public List<String> getValueList() {
		return Arrays.asList(values);
	}

	/**
	 * Get the values as array.
	 * 
	 * @return the values as array
	 */
	public String[] getValue() {
		return values;
	}

	@Override
	public ListDescriptor<I> clone() {
		return this;
	}

	@Override
	public String getUniqueString() {
		return Strings.join(SEPARATOR, values);
	}

	@Override
	public double getFactValue(final IDataRecord record) {
		return values.length;
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
