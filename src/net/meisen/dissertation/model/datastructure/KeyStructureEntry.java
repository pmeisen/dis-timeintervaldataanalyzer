package net.meisen.dissertation.model.datastructure;

public class KeyStructureEntry extends StructureEntry {

	public KeyStructureEntry(final String name) {
		this(name, -1);
	}

	public KeyStructureEntry(final int position) {
		this(null, position);
	}

	public KeyStructureEntry(final String name, final int position) {
		super(name, position);
	}
}
