package net.meisen.dissertation.model.datastructure;

import java.util.UUID;

public abstract class StructureEntry {
	private final String name;
	private final int position;

	public StructureEntry(final String name) {
		this(name, -1);
	}

	public StructureEntry(final int position) {
		this(null, position);
	}

	public StructureEntry(final String name, final int position) {
		this.name = name;
		this.position = position;
	}

	public int getPosition() {
		return position;
	}

	public String getName() {
		return name;
	}
}
