package net.meisen.dissertation.impl.parser.query.select.group;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.List;

import net.meisen.dissertation.impl.parser.query.select.DescriptorValue;
import net.meisen.general.genmisc.types.Strings;

/**
 * An filter defined for a group.
 * 
 * @author pmeisen
 * 
 */
public class GroupFilter {
	private final List<DescriptorValue> filter = new ArrayList<DescriptorValue>();

	/**
	 * Default constructor defines no filter at all.
	 */
	public GroupFilter() {
		// nothing to do
	}

	/**
	 * Constructor to define the values of the filter via {@code array}.
	 * 
	 * @param values
	 *            the values of the filter
	 */
	public GroupFilter(final String... values) {
		setValues(values);
	}

	/**
	 * Constructor to define the values of the filter via {@code Collection}.
	 * 
	 * @param values
	 *            the values of the filter
	 */
	public GroupFilter(final Collection<String> values) {
		setValues(values);
	}

	/**
	 * Remove all the values and add the once specified.
	 * 
	 * @param values
	 *            the values to be set as filter.
	 */
	public void setValues(final String[] values) {
		if (values == null) {
			return;
		}

		setValues(Arrays.asList(values));
	}

	/**
	 * Remove all the values and add the once specified.
	 * 
	 * @param values
	 *            the values to be set as filter.
	 */
	public void setValues(final Collection<String> values) {
		filter.clear();
		for (final String val : values) {
			filter.add(new DescriptorValue(val));
		}
	}

	/**
	 * Gets the values defined for the filter.
	 * 
	 * @return the values of the filter
	 */
	public List<DescriptorValue> getValues() {
		return Collections.unmodifiableList(filter);
	}

	/**
	 * Gets the amount of defined values for the filter.
	 * 
	 * @return the amount of defined values for the filter
	 */
	public int getAmountOfValues() {
		return filter.size();
	}

	@Override
	public String toString() {
		return Strings.join(",", filter);
	}

	/**
	 * Gets the defined value for a specific position.
	 * 
	 * @param position
	 *            the position to get the {@code DescriptorValue} for
	 * @return the {@code DescriptorValue} associated
	 * 
	 * @throws IndexOutOfBoundsException
	 *             if the index is out of range (index < 0 || index >= size())
	 */
	public DescriptorValue getValue(final int position)
			throws IndexOutOfBoundsException {
		return filter.get(position);
	}
}
