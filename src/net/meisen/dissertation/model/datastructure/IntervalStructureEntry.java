package net.meisen.dissertation.model.datastructure;

public class IntervalStructureEntry extends StructureEntry {

	public static enum IntervalStructureEntryType {
		START, END
	}

	private final IntervalStructureEntryType type;

	public IntervalStructureEntry(final IntervalStructureEntryType type,
			final String name) {
		this(type, name, -1);
	}

	public IntervalStructureEntry(final IntervalStructureEntryType type,
			final int position) {
		this(type, null, position);
	}

	public IntervalStructureEntry(final IntervalStructureEntryType type,
			final String name, final int position) {
		super(name, position);

		this.type = type;
	}

	public IntervalStructureEntryType getType() {
		return type;
	}
}
