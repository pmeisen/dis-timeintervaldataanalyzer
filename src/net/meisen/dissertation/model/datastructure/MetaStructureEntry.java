package net.meisen.dissertation.model.datastructure;

import net.meisen.general.genmisc.types.Objects;

/**
 * A {@code MetaStructureEntry} binds the entry to a {@code DescriptorModel}.
 * 
 * @author pmeisen
 * 
 */
public class MetaStructureEntry extends StructureEntry {

	private final String descriptorModel;

	/**
	 * A {@code MetaStructureEntry} which is based on the {@code name} of a data
	 * element.
	 * 
	 * @param descriptorModel
	 *            the id of the {@code DescriptorModel} which is bound to
	 *            {@code this} entry
	 * @param name
	 *            the name of the field of the {@code DataRecord}, should not be
	 *            {@code null}
	 */
	public MetaStructureEntry(final String descriptorModel, final String name) {
		this(descriptorModel, name, -1);
	}

	/**
	 * A {@code MetaStructureEntry} which is based on a {@code position} of a
	 * data element.
	 * 
	 * @param descriptorModel
	 *            the id of the {@code DescriptorModel} which is bound to
	 *            {@code this} entry
	 * @param position
	 *            the position within the {@code DataRecord}, should be a value
	 *            larger than {@code 0}
	 */
	public MetaStructureEntry(final String descriptorModel, final int position) {
		this(descriptorModel, null, position);
	}

	/**
	 * A {@code MetaStructureEntry} which is based on a {@code position} and
	 * {@code name} of a data element.
	 * 
	 * @param descriptorModel
	 *            the id of the {@code DescriptorModel} which is bound to
	 *            {@code this} entry
	 * @param name
	 *            the name of the field of the {@code DataRecord}, can be
	 *            {@code null} if a position larger than {@code 0} is specified
	 * @param position
	 *            the position within the {@code DataRecord}, can a value
	 *            smaller than {@code 1} if a name is specified
	 */
	public MetaStructureEntry(final String descriptorModel, final String name,
			final int position) {
		super(name, position);

		this.descriptorModel = descriptorModel;
	}

	/**
	 * Gets the id of the {@code DescriptorModel} which is bound to {@code this}
	 * entry.
	 * 
	 * @return the id of the {@code DescriptorModel} which is bound to
	 *         {@code this} entry
	 */
	public String getDescriptorModel() {
		return descriptorModel;
	}

	@Override
	public boolean equals(final Object o) {
		if (super.equals(o)) {
			final MetaStructureEntry e = (MetaStructureEntry) o;
			return Objects.equals(getDescriptorModel(), e.getDescriptorModel());
		} else {
			return false;
		}
	}
}
