package net.meisen.dissertation.impl.parser.query.select.group;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Set;

import net.meisen.general.genmisc.types.Strings;

/**
 * Tests the implementation of the {@code GroupExpression}.
 * 
 * @author pmeisen
 * 
 */
public class GroupExpression {
	private final Set<String> descriptorIds = new LinkedHashSet<String>();
	private final List<GroupExclusion> exclusions = new ArrayList<GroupExclusion>();

	/**
	 * Constructor to create a group expression.
	 */
	public GroupExpression() {
		// nothing to do
	}

	/**
	 * Constructor to create a group expression.
	 * 
	 * @param descriptorsIds
	 *            the list of identifiers of the {@code DescriptorModel} to be
	 *            grouped
	 */
	public GroupExpression(final String... descriptorsIds) {
		setDescriptors(descriptorsIds);
	}

	/**
	 * Constructor to create a group expression.
	 * 
	 * @param descriptorsIds
	 *            the list of identifiers of the {@code DescriptorModel} to be
	 *            grouped
	 */
	public GroupExpression(final Collection<String> descriptorsIds) {
		setDescriptors(descriptorsIds);
	}

	/**
	 * Sets the descriptors, i.e. all the actual added descriptors are removed.
	 * 
	 * @param descriptorIds
	 *            the descriptors to be set
	 */
	public void setDescriptors(final String... descriptorIds) {
		if (descriptorIds == null) {
			return;
		}

		setDescriptors(Arrays.asList(descriptorIds));
	}

	/**
	 * Sets the descriptors, i.e. all the actual added descriptors are removed.
	 * 
	 * @param descriptorIds
	 *            the descriptors to be set
	 */
	public void setDescriptors(final Collection<String> descriptorIds) {
		if (descriptorIds == null) {
			return;
		}

		this.descriptorIds.clear();
		addDescriptors(descriptorIds);
	}

	/**
	 * Adds all the specified descriptors.
	 * 
	 * @param descriptorIds
	 *            the descriptors' identifiers to be added
	 * 
	 * @return {@code true} if all descriptors' identifiers were added,
	 *         otherwise {@code false}
	 */
	public boolean addDescriptors(final String... descriptorIds) {
		if (descriptorIds == null) {
			return true;
		}

		return addDescriptors(Arrays.asList(descriptorIds));
	}

	/**
	 * Adds all the specified descriptors.
	 * 
	 * @param descriptorIds
	 *            the descriptors' identifiers to be added
	 * 
	 * @return {@code true} if all descriptors' identifiers were added,
	 *         otherwise {@code false}
	 */
	public boolean addDescriptors(final Collection<String> descriptorIds) {
		if (descriptorIds == null) {
			return true;
		}

		boolean ret = true;
		for (final String descriptorId : descriptorIds) {
			if (!addDescriptor(descriptorId)) {
				ret = false;
			}
		}

		return ret;
	}

	/**
	 * Adds the descriptor's identifier to the list of descriptors defining the
	 * group. If the descriptor was already added, it will not be added again
	 * and {@code false} will be returned.
	 * 
	 * @param descriptorId
	 *            the identifier of the descriptor to be added
	 * 
	 * @return {@code true} if the descriptor was added, otherwise {@code false}
	 */
	public boolean addDescriptor(final String descriptorId) {
		if (descriptorId == null) {
			return false;
		}
		return descriptorIds.add(descriptorId);
	}

	/**
	 * Gets all the specified descriptors.
	 * 
	 * @return the descriptors making up the group, ordered by definition
	 */
	public Set<String> getDescriptors() {
		return Collections.unmodifiableSet(descriptorIds);
	}

	/**
	 * Adds an exclusion definition for the group.
	 * 
	 * @param values
	 *            the values to be excluded
	 */
	public void addExclusion(final String... values) {
		if (values == null) {
			return;
		}

		addExclusion(Arrays.asList(values));
	}

	/**
	 * Adds an exclusion definition for the group.
	 * 
	 * @param values
	 *            the values to be excluded
	 */
	public void addExclusion(final Collection<String> values) {

		// create a dedicated list for the exclusion
		final GroupExclusion exclusion = new GroupExclusion();
		exclusion.setValues(values);

		// add the exclusion
		addExclusion(exclusion);
	}

	/**
	 * Adds an exclusion definition for the group.
	 * 
	 * @param exclusion
	 *            the exclusion to be added
	 */
	public void addExclusion(final GroupExclusion exclusion) {
		if (exclusion != null) {
			exclusions.add(exclusion);
		}
	}

	/**
	 * Removes all the defined exclusions.
	 */
	public void removeExclusions() {
		exclusions.clear();
	}

	/**
	 * Gets the defined exclusions.
	 * 
	 * @return the exclusions
	 */
	public List<GroupExclusion> getExclusions() {
		return Collections.unmodifiableList(exclusions);
	}

	/**
	 * Checks if the specified {@code GroupExpression} seems to be valid, i.e.
	 * well-defined within itself.
	 * 
	 * @return {@code true} if the expression is well-defined, otherwise
	 *         {@code false}
	 */
	public boolean isValid() {
		final int expectedSize = descriptorIds.size();

		for (final GroupExclusion exclusion : exclusions) {
			if (expectedSize < exclusion.getAmountOfValues()) {
				return false;
			}
		}

		return true;
	}

	/**
	 * Gets the position of a {@code DescriptorModel} within the group.
	 * 
	 * @param descId
	 *            the identifier of the {@code DescriptorModel} to be found
	 * 
	 * @return the position, or {@code -1} if it cannot be found
	 */
	public int getPosition(final String descId) {
		if (descId == null) {
			return -1;
		}

		return net.meisen.general.genmisc.collections.Collections.getPosition(
				descriptorIds, descId);
	}

	@Override
	public String toString() {
		return "["
				+ Strings.join(",", descriptorIds)
				+ "]"
				+ (exclusions.size() > 0 ? " excluding {"
						+ Strings.join(";", exclusions) + "}" : "");
	}
}
