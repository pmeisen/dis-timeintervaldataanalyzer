package net.meisen.dissertation.model.persistence;

import java.util.Arrays;
import java.util.regex.Pattern;

import net.meisen.general.genmisc.types.Objects;
import net.meisen.general.genmisc.types.Strings;

/**
 * A group is a path-like structure to define a path to a specific element.
 * 
 * @author pmeisen
 * 
 */
public class Group {
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
		this.items = items == null ? new String[0] : items;
	}

	/**
	 * Creates a new {@code Group} by appending the specified item.
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
	 * Creates a {@code Group} based on a string.
	 * 
	 * @param group
	 *            the string representation of a group
	 * 
	 * @return the created {@code Group}
	 */
	public static Group createFromString(final String group) {
		return createFromString(group, DEF_SEPARTOR);
	}

	/**
	 * Creates a {@code Group} based on a string.
	 * 
	 * @param group
	 *            the string representation of a group
	 * @param separator
	 *            the separator used within the string
	 * 
	 * @return the created {@code Group}
	 */
	public static Group createFromString(final String group,
			final String separator) {
		if (group == null) {
			return new Group();
		} else {
			return new Group(group.split(Pattern.quote(separator)));
		}
	}
}
