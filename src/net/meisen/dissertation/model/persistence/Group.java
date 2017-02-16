package net.meisen.dissertation.model.persistence;

import java.util.Arrays;
import java.util.Iterator;
import java.util.regex.Pattern;

import net.meisen.general.genmisc.types.Objects;
import net.meisen.general.genmisc.types.Strings;

/**
 * A group is a path-like structure to define a path to a specific element.
 * 
 * @author pmeisen
 * 
 */
public class Group implements Iterable<Group> {
	private static final String VALIDATION_EXPR = "[A-Za-z0-9_\\-]+";

	/**
	 * Default separator used if none is defined.
	 */
	public final static String DEF_SEPARTOR = "/";

	private final String[] items;

	/**
	 * Creates an empty group.
	 */
	public Group() {
		this((String[]) null);
	}

	/**
	 * Creates a group with the specified path.
	 * 
	 * @param items
	 *            the items which make up the path
	 */
	public Group(final String... items) {
		// validate the selected items
		validate(items);

		this.items = items == null ? new String[0] : items;
	}

	/**
	 * Gets the parent group of {@code this}.
	 * 
	 * @return the parent group of {@code this}
	 */
	public Group getParentGroup() {
		if (items.length == 0) {
			return null;
		} else {
			return new Group(Arrays.copyOfRange(items, 0, items.length - 1));
		}
	}

	/**
	 * Validate the strings used as group.
	 * 
	 * @param items
	 *            the array of strings
	 */
	protected void validate(final String[] items) {
		if (items == null || (items.length == 1 && "".equals(items[0]))) {
			return;
		} else {
			for (final String item : items) {
				if (item == null) {
					throw new IllegalArgumentException("A group cannot be null.");
				} else if (!"".equals(item) && !item.matches(VALIDATION_EXPR)) {
					throw new IllegalArgumentException(
							"The group '" + item + "' is invalid considering the validation expression '" +
									VALIDATION_EXPR + "'.");
				}
			}
		}
	}

	/**
	 * Creates a new {@code SelectGroup} by appending the specified item.
	 * 
	 * @param item
	 *            the item to append
	 * 
	 * @return the new group
	 */
	public Group append(final String item) {
		final String[] newItems = Arrays.copyOf(items, items.length + 1);
		newItems[items.length] = item;

		return new Group(newItems);
	}

	/**
	 * Checks if the group has any items.
	 * 
	 * @return {@code true} if the group has items, otherwise {@code false}
	 */
	public boolean hasItems() {
		return items.length > 0;
	}

	@Override
	public boolean equals(final Object o) {
		if (o == this) {
			return true;
		} else if (o instanceof Group) {
			final Group g = (Group) o;
			if (g.items.length == items.length) {
				for (int i = 0; i < items.length; i++) {
					if (!items[i].equals(g.items[i])) {
						return false;
					}
				}

				return true;
			} else {
				return false;
			}
		} else {
			return false;
		}
	}

	@Override
	public int hashCode() {
		return Objects.generateHashCode(7, 13, (Object[]) items);
	}

	/**
	 * Generates a string with the specified separator.
	 * 
	 * @param separator
	 *            the separator to be used
	 * 
	 * @return the created string
	 */
	public String toString(final String separator) {
		return Strings.join(separator, items);
	}

	@Override
	public String toString() {
		return toString(DEF_SEPARTOR);
	}

	/**
	 * Checks if the group is empty, i.e. the parent of all groups.
	 * 
	 * @return {@code true} if the group is empty, otherwise {@code false}
	 */
	public boolean isEmpty() {
		return !hasItems();
	}

	/**
	 * Creates a {@code SelectGroup} based on a string.
	 * 
	 * @param group
	 *            the string representation of a group
	 * 
	 * @return the created {@code SelectGroup}
	 */
	public static Group createFromString(final String group) {
		return createFromString(group, DEF_SEPARTOR);
	}

	/**
	 * Creates a {@code SelectGroup} based on a string.
	 * 
	 * @param group
	 *            the string representation of a group
	 * @param separator
	 *            the separator used within the string
	 * 
	 * @return the created {@code SelectGroup}
	 */
	public static Group createFromString(final String group,
			final String separator) {
		if (group == null) {
			return new Group();
		} else {
			return new Group(group.split(Pattern.quote(separator)));
		}
	}

	@Override
	public Iterator<Group> iterator() {
		return new Iterator<Group>() {
			int i = 0;

			@Override
			public boolean hasNext() {
				return i < items.length;
			}

			@Override
			public Group next() {
				i++;

				final String[] newItems = Arrays.copyOfRange(items, 0, i);
				return new Group(newItems);
			}

			@Override
			public void remove() {
				throw new UnsupportedOperationException(
						"Cannot remove an element!");
			}

		};
	}

	/**
	 * Gets the zero-based part of the group. This method returns for the group
	 * {@code "first", "second"}, {@code "first"} if {@code 0} is passed as
	 * position and {@code "second"} if {@code 1} is passed.
	 * 
	 * @param pos
	 *            the position (zero-based) to retrieve the group part of this
	 *            group for
	 * @return the value of the group part at the specified position
	 */
	public String getPart(final int pos) {
		return items[pos];
	}

	/**
	 * Gets the amount of parts making up the group.
	 * 
	 * @return the amount of parts making up the group
	 */
	public int size() {
		return items.length;
	}

	/**
	 * Creates a sub-group of {@code this} by removing the {@code prefix}.
	 * 
	 * @param prefix
	 *            the prefix to be removed
	 *            
	 * @return the sub-group, or {@code null} if the prefix is not an actual
	 *         prefix
	 */
	public Group removePrefix(final Group prefix) {

		// if the prefix is longer it cannot be a prefix for sure
		if (prefix.size() > size() || isEmpty()) {
			return null;
		}

		// validate
		for (int i = 0; i < prefix.size(); i++) {
			final String prefixValue = prefix.getPart(i);
			if (!Objects.equals(prefixValue, getPart(i))) {
				return null;
			}
		}

		return new Group(Arrays.copyOfRange(items, prefix.size(), size()));
	}
}
