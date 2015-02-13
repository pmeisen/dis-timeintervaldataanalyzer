package net.meisen.dissertation.impl.parser.query.select.evaluator;

import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

import net.meisen.dissertation.model.indexes.datarecord.slices.Bitmap;

/**
 * A {@code GroupResult} represents the result retrieved from a
 * {@code TidaModel} for a specified {@code GroupExpression}.
 * 
 * @author pmeisen
 * 
 * @see GroupEvaluator
 * 
 */
public class GroupResult implements Iterable<GroupResultEntry> {
	private Map<List<String>, GroupResultEntry> results = new LinkedHashMap<List<String>, GroupResultEntry>();

	/**
	 * Adds the specified {@code group} and it's determined {@code bitmap} to
	 * the result.
	 * 
	 * @param group
	 *            the group the {@code bitmap} is determined for
	 * @param bitmap
	 *            the determined bitmap to be added to the result
	 */
	public void add(final List<String> group, final Bitmap bitmap) {
		if (group == null) {
			throw new NullPointerException("Cannot add a null group");
		}

		final GroupResultEntry entry = new GroupResultEntry(group, bitmap);
		results.put(entry.getGroupAsList(), entry);
	}

	/**
	 * Gets the size of the result, i.e. the amount of groups.
	 * 
	 * @return the size of the result
	 */
	public int size() {
		return results.size();
	}

	/**
	 * Gets the different entries within the result.
	 * 
	 * @return the different entries within the result
	 */
	public Collection<GroupResultEntry> entries() {
		return Collections.unmodifiableCollection(results.values());
	}

	/**
	 * Gets a specific entry for a specified {@code group}.
	 * 
	 * @param group
	 *            the group to get the entry for
	 * 
	 * @return the entry for the specified {@code group}, {@code null} is
	 *         returned if no entry was found
	 */
	public GroupResultEntry getEntry(final String... group) {
		return getEntry(Arrays.asList(group));
	}

	/**
	 * Gets a specific entry for a specified {@code group}.
	 * 
	 * @param group
	 *            the group to get the entry for
	 * 
	 * @return the entry for the specified {@code group}, {@code null} is
	 *         returned if no entry was found
	 */
	public GroupResultEntry getEntry(final List<String> group) {
		return results.get(group);
	}

	@Override
	public Iterator<GroupResultEntry> iterator() {
		return results.values().iterator();
	}

	@Override
	public String toString() {
		return results.values().toString();
	}

	/**
	 * Checks if the group is based on one single group.
	 * 
	 * @return {@code true} if only one group is defined, otherwise
	 *         {@code false}
	 */
	public boolean isSingleGroup() {
		return results.size() == 1;
	}

	/**
	 * Gets the one and only group defined, i.e. only returns a value if
	 * {@code #isSingleGroup()} returns {@code true}.
	 * 
	 * @return the single {@code GroupResultEntry} or {@code null} if the
	 *         definition is not a single group
	 */
	public GroupResultEntry getSingleGroupEntry() {
		return isSingleGroup() ? results.values().iterator().next() : null;
	}
}
