package net.meisen.dissertation.model.datastructure;

/**
 * A {@code MetaStructureEntry} binds the entry to a {@code Descriptor}.
 * 
 * @author pmeisen
 * 
 */
public class MetaStructureEntry extends StructureEntry {

	private final String descriptor;

	/**
	 * A {@code MetaStructureEntry} which is based on the {@code name} of a data
	 * element.
	 * 
	 * @param descriptor
	 *            the id of the {@code Descriptor} which is bound to
	 *            {@code this} entry
	 * @param name
	 *            the name to be based on, should not be {@code null}
	 */
	public MetaStructureEntry(final String descriptor, final String name) {
		this(descriptor, name, -1);
	}

	/**
	 * A {@code MetaStructureEntry} which is based on a {@code position} of a
	 * data element.
	 * 
	 * @param descriptor
	 *            the id of the {@code Descriptor} which is bound to
	 *            {@code this} entry
	 * @param position
	 *            the position to be based on, should be a value larger than
	 *            {@code 0}
	 */
	public MetaStructureEntry(final String descriptor, final int position) {
		this(descriptor, null, position);
	}

	/**
	 * A {@code MetaStructureEntry} which is based on a {@code position} and
	 * {@code name} of a data element.
	 * 
	 * @param descriptor
	 *            the id of the {@code Descriptor} which is bound to
	 *            {@code this} entry
	 * @param name
	 *            the name to be based on, can be {@code null} if a position
	 *            larger than {@code 0} is specified
	 * @param position
	 *            the position to be based on, can a value smaller than
	 *            {@code 1} if a name is specified
	 */
	public MetaStructureEntry(final String descriptor, final String name,
			final int position) {
		super(name, position);

		this.descriptor = descriptor;
	}

	/**
	 * Gets the id of the {@code Descriptor} which is bound to {@code this}
	 * entry.
	 * 
	 * @return the id of the {@code Descriptor} which is bound to {@code this}
	 *         entry
	 */
	public String getDescriptor() {
		return descriptor;
	}
}
