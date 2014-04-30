package net.meisen.dissertation.model.persistence;

import net.meisen.general.genmisc.types.Objects;

/**
 * An {@code Identifier} is used to identify a persisted instance.
 * 
 * @author pmeisen
 * 
 */
public class Identifier {
	private static final String VALIDATION_EXPR = "[A-Za-z0-9_\\-\\.]+";

	private final Group group;
	private final String id;
	private String comment;

	/**
	 * Creates an identifier with the specified {@code id} in the specified
	 * group.
	 * 
	 * @param id
	 *            the id
	 * @param items
	 *            the items of the {@code SelectGroup}
	 * 
	 * @see Group
	 */
	public Identifier(final String id, final String... items) {
		this(id, new Group(items));
	}

	/**
	 * Creates an identifier with the specified {@code id} in the specified
	 * {@code group}.
	 * 
	 * @param id
	 *            the id
	 * @param group
	 *            the group
	 * 
	 * @see Group
	 */
	public Identifier(final String id, final Group group) {
		validate(id);

		this.id = id;
		this.group = group == null ? new Group() : group;
	}

	/**
	 * Validates the id to be used
	 * 
	 * @param id
	 *            the id to be used
	 */
	protected void validate(final String id) {
		if (id == null) {
			return;
		} else if (!id.matches(VALIDATION_EXPR)) {
			throw new IllegalArgumentException("The id '" + id
					+ "' is invalid considering the validation expression '"
					+ VALIDATION_EXPR + "'.");
		}
	}

	/**
	 * Gets the group of the {@code Identifier}.
	 * 
	 * @return the group
	 */
	public Group getGroup() {
		return group;
	}

	/**
	 * Gets the id of the {@code Identifier}.
	 * 
	 * @return the id
	 */
	public String getId() {
		return id;
	}

	@Override
	public boolean equals(final Object o) {
		if (o == this) {
			return true;
		} else if (o instanceof Identifier) {
			final Identifier i = (Identifier) o;
			return Objects.equals(i.id, id) && i.group.equals(group);
		} else {
			return false;
		}
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
		return group.toString(separator) + (group.hasItems() ? separator : "")
				+ id;
	}

	@Override
	public String toString() {
		return toString(Group.DEF_SEPARTOR);
	}

	/**
	 * Creates an {@code Identifier} based on a string.
	 * 
	 * @param identifier
	 *            the string representation of the identifier
	 * 
	 * @return the created {@code Identifier}
	 */
	public static Identifier createFromString(final String identifier) {
		return createFromString(identifier, Group.DEF_SEPARTOR);
	}

	/**
	 * Creates an {@code Identifier} based on a string.
	 * 
	 * @param identifier
	 *            the string representation of the identifier
	 * @param separator
	 *            the separator used within the string
	 * 
	 * @return the created {@code Identifier}
	 */
	public static Identifier createFromString(final String identifier,
			final String separator) {

		if (identifier == null) {
			return new Identifier(identifier);
		}

		int pos = identifier.lastIndexOf(separator);
		if (pos < 0) {
			return new Identifier(identifier);
		} else {
			final String id = identifier.substring(pos + 1);
			final String group = identifier.substring(0, pos);

			return new Identifier(id, Group.createFromString(group, separator));
		}
	}

	/**
	 * Get the specified comment for the identifier.
	 * 
	 * @return the comment for the identifier
	 */
	public String getComment() {
		return comment;
	}

	/**
	 * Sets the comment for the identifier
	 * 
	 * @param comment
	 *            the comment for the identifier
	 */
	public void setComment(final String comment) {
		this.comment = comment;
	}

	@Override
	public int hashCode() {
		return Objects.generateHashCode(3, 17, new Object[] { id, group });
	}
}
