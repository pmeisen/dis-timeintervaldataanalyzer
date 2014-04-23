package net.meisen.dissertation.model.datastructure;


/**
 * An {@code StructureEntry} which marks specific data to be a part of or a key.
 * All {@code KeyStructureEntry} instances of a {@code DataStructure} form the
 * key and should be unique.
 * 
 * @author pmeisen
 * 
 */
public class KeyStructureEntry extends StructureEntry {

	/**
	 * A {@code KeyStructureEntry} which is based on the {@code name} of a data
	 * element
	 * 
	 * @param name
	 *            the name to be based on, should not be {@code null}
	 */
	public KeyStructureEntry(final String name) {
		this(name, -1);
	}

	/**
	 * A {@code KeyStructureEntry} which is based on a {@code position} of a
	 * data element.
	 * 
	 * @param position
	 *            the position to be based on, should be a value larger than
	 *            {@code 0}
	 */
	public KeyStructureEntry(final int position) {
		this(null, position);
	}

	/**
	 * A {@code KeyStructureEntry} which is based on a {@code position} and
	 * {@code name} of a data element.
	 * 
	 * @param name
	 *            the name to be based on, can be {@code null} if a position
	 *            larger than {@code 0} is specified
	 * @param position
	 *            the position to be based on, can a value smaller than
	 *            {@code 1} if a name is specified
	 */
	public KeyStructureEntry(final String name, final int position) {
		super(name, position);
	}

	@Override
	public boolean equals(final Object o) {
		if (super.equals(o)) {
			return true;
		} else {
			return false;
		}
	}
}
