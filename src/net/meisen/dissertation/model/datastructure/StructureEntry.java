package net.meisen.dissertation.model.datastructure;

import net.meisen.general.genmisc.types.Objects;

/**
 * A {@code StructureEntry} defines an entry within a {@code DataStructure}. It
 * defines the semantics (what is the entry there for) and the binding (where to
 * get the data from).
 * 
 * @author pmeisen
 * 
 */
public abstract class StructureEntry {
	private final String name;
	private final int position;

	/**
	 * A {@code StructureEntry} which is based on the {@code name} of a data
	 * element
	 * 
	 * @param name
	 *            the name to be based on, should not be {@code null}
	 */
	public StructureEntry(final String name) {
		this(name, -1);
	}

	/**
	 * A {@code StructureEntry} which is based on a {@code position} of a data
	 * element.
	 * 
	 * @param position
	 *            the position to be based on, should be a value larger than
	 *            {@code 0}
	 */
	public StructureEntry(final int position) {
		this(null, position);
	}

	/**
	 * A {@code StructureEntry} which is based on a {@code position} and
	 * {@code name} of a data element.
	 * 
	 * @param name
	 *            the name to be based on, can be {@code null} if a position
	 *            larger than {@code 0} is specified
	 * @param position
	 *            the position to be based on, can a value smaller than
	 *            {@code 1} if a name is specified
	 */
	public StructureEntry(final String name, final int position) {
		this.name = name;
		this.position = position < 1 ? -1 : position;
	}

	/**
	 * Gets the defined position of this {@code StructureEntry}. The value will
	 * be {@code -1} or a value larger than {@code 1}.
	 * 
	 * @return the defined position
	 */
	public int getPosition() {
		return position;
	}

	/**
	 * Gets the defined name of this {@code StructureEntry}. The value can be
	 * {@code null} if no name was specified.
	 * 
	 * @return the defined name
	 */
	public String getName() {
		return name;
	}

	@Override
	public boolean equals(final Object o) {
		if (o == this) {
			return true;
		} else if (o == null) {
			return false;
		} else if (getClass().equals(o.getClass())) {
			final StructureEntry e = (StructureEntry) o;
			return getPosition() == e.getPosition()
					&& Objects.equals(getName(), e.getName());
		} else {
			return false;
		}
	}

	@Override
	public int hashCode() {
		if (position > 0) {
			return position;
		} else {
			return name.hashCode();
		}
	}

	@Override
	public String toString() {
		return getClass().getSimpleName() + ": "
				+ (name == null ? position : name);
	}
}
