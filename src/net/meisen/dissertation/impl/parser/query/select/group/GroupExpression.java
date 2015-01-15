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
	private final Set<Object> selectors = new LinkedHashSet<Object>();
	private final List<GroupFilter> inclusions = new ArrayList<GroupFilter>();
	private final List<GroupFilter> exclusions = new ArrayList<GroupFilter>();

	/**
	 * Constructor to create a group expression.
	 */
	public GroupExpression() {
		// nothing to do
	}

	/**
	 * Constructor to create a group expression.
	 * 
	 * @param selectors
	 *            the list of identifiers to be grouped
	 */
	public GroupExpression(final Object... selectors) {
		setSelectors(selectors);
	}

	/**
	 * Constructor to create a group expression.
	 * 
	 * @param selectors
	 *            the list of identifiers to be grouped
	 */
	public GroupExpression(final Collection<Object> selectors) {
		setSelectors(selectors);
	}

	/**
	 * Sets the selectors for the expression.
	 * 
	 * @param selectors
	 *            the selectors to be set, all other selectors are removed
	 */
	public void setSelectors(final Object... selectors) {
		if (selectors == null) {
			return;
		}

		setSelectors(Arrays.asList(selectors));
	}

	/**
	 * Sets the selectors for the expression.
	 * 
	 * @param selectors
	 *            the selectors to be set, all other selectors are removed
	 */
	public void setSelectors(final Collection<Object> selectors) {
		if (selectors == null) {
			return;
		}

		this.selectors.clear();
		addSelectors(selectors);
	}

	/**
	 * Adds all the specified descriptors.
	 * 
	 * @param selectors
	 *            the selectors to be added
	 * 
	 * @return {@code true} if all selectors identifiers were added, otherwise
	 *         {@code false}
	 */
	public boolean addSelectors(final Object... selectors) {
		if (selectors == null) {
			return true;
		}

		return addSelectors(Arrays.asList(selectors));
	}

	/**
	 * Adds all the specified descriptors.
	 * 
	 * @param selectors
	 *            the selectors to be added
	 * 
	 * @return {@code true} if all selectors identifiers were added, otherwise
	 *         {@code false}
	 */
	public boolean addSelectors(final Collection<Object> selectors) {
		if (selectors == null) {
			return true;
		}

		boolean ret = true;
		for (final Object selector : selectors) {
			if (!addSelector(selector)) {
				ret = false;
			}
		}

		return ret;
	}

	/**
	 * Adds the selector defining the group. If the selector was already added,
	 * it will not be added again and {@code false} will be returned.
	 * 
	 * @param selector
	 *            the selector to be added
	 * 
	 * @return {@code true} if the selector was added, otherwise {@code false}
	 */
	public boolean addSelector(final Object selector) {
		if (selector == null) {
			return false;
		}
		return selectors.add(selector);
	}

	/**
	 * Gets all the specified (ordered) selectors.
	 * 
	 * @return the selectors making up the group, ordered by definition
	 */
	public Set<Object> getSelectors() {
		return Collections.unmodifiableSet(selectors);
	}

	/**
	 * Adds an inclusion definition for the group.
	 * 
	 * @param values
	 *            the values to be included
	 */
	public void addInclusion(final String... values) {
		if (values == null) {
			return;
		}

		addInclusion(Arrays.asList(values));
	}

	/**
	 * Adds an inclusion definition for the group.
	 * 
	 * @param values
	 *            the values to be included
	 */
	public void addInclusion(final Collection<String> values) {

		// create a dedicated list for the exclusion
		final GroupFilter inclusion = new GroupFilter();
		inclusion.setValues(values);

		// add the exclusion
		addInclusion(inclusion);
	}

	/**
	 * Adds an exclusion definition for the group.
	 * 
	 * @param inclusion
	 *            the inclusion to be added
	 */
	public void addInclusion(final GroupFilter inclusion) {
		if (inclusion != null) {
			inclusions.add(inclusion);
		}
	}

	/**
	 * Removes all the defined inclusions.
	 */
	public void removeInclusions() {
		inclusions.clear();
	}

	/**
	 * Gets the defined inclusions.
	 * 
	 * @return the inclusions
	 */
	public List<GroupFilter> getInclusions() {
		return Collections.unmodifiableList(inclusions);
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
		final GroupFilter exclusion = new GroupFilter();
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
	public void addExclusion(final GroupFilter exclusion) {
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
	public List<GroupFilter> getExclusions() {
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
		final int expectedSize = selectors.size();

		for (final GroupFilter inclusion : inclusions) {
			if (expectedSize < inclusion.getAmountOfValues()) {
				return false;
			}
		}

		for (final GroupFilter exclusion : exclusions) {
			if (expectedSize < exclusion.getAmountOfValues()) {
				return false;
			}
		}

		return true;
	}

	/**
	 * Gets the position of an expression within the group.
	 * 
	 * @param selector
	 *            the selector to be found
	 * 
	 * @return the position, or {@code -1} if it cannot be found
	 */
	public int getPosition(final Object selector) {
		if (selector == null) {
			return -1;
		}

		return net.meisen.general.genmisc.collections.Collections.getPosition(
				selectors, selector);
	}

	@Override
	public String toString() {
		return "["
				+ Strings.join(",", selectors)
				+ "]"
				+ (exclusions.size() > 0 ? " excluding {"
						+ Strings.join(";", exclusions) + "}" : "");
	}
}
