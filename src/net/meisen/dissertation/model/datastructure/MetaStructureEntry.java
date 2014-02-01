package net.meisen.dissertation.model.datastructure;

public class MetaStructureEntry extends StructureEntry {

	private final String descriptor;

	public MetaStructureEntry(final String descriptor, final String name) {
		this(descriptor, name, -1);
	}

	public MetaStructureEntry(final String descriptor, final int position) {
		this(descriptor, null, position);
	}

	public MetaStructureEntry(final String descriptor, final String name,
			final int position) {
		super(name, position);

		this.descriptor = descriptor;
	}

	public String getDescriptor() {
		return descriptor;
	}
}
