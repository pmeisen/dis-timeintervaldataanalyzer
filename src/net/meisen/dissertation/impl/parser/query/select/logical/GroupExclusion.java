package net.meisen.dissertation.impl.parser.query.select.logical;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.List;

import net.meisen.dissertation.impl.parser.query.select.DescriptorValue;
import net.meisen.general.genmisc.types.Strings;

/**
 * An exclusion defined for a group.
 * 
 * @author pmeisen
 * 
 */
public class GroupExclusion {
	private final List<DescriptorValue> exclusion = new ArrayList<DescriptorValue>();

	/**
	 * Default constructor defines no exclusion at all.
	 */
	public GroupExclusion() {
		// nothing to do
	}

	/**
	 * Constructor to define the values of the exclusion via {@code array}.
	 * 
	 * @param values
	 *            the values of the exclusion
	 */
	public GroupExclusion(final String... values) {
		setValues(values);
	}

	/**
	 * Constructor to define the values of the exclusion via {@code Collection}.
	 * 
	 * @param values
	 *            the values of the exclusion
	 */
	public GroupExclusion(final Collection<String> values) {
		setValues(values);
	}

	/**
	 * Remove all the values and add the once specified.
	 * 
	 * @param values
	 *            the values to be set as exclusion.
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
	 *            the values to be set as exclusion.
	 */
	public void setValues(final Collection<String> values) {
		exclusion.clear();
		for (final String val : values) {
			exclusion.add(new DescriptorValue(val));
		}
	}

	/**
	 * Gets the values defined for the exclusion.
	 * 
	 * @return the values of the exclusion
	 */
	public List<DescriptorValue> getValues() {
		return Collections.unmodifiableList(exclusion);
	}

	/**
	 * Gets the amount of defined values for the exclusion.
	 * 
	 * @return the amount of defined values for the exclusion
	 */
	public int getAmountOfValues() {
		return exclusion.size();
	}

	@Override
	public String toString() {
		return Strings.join(",", exclusion);
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
		return exclusion.get(position);
	}
}
