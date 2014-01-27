package net.meisen.dissertation.model.datasets;

import java.util.UUID;

public class SingleStaticDataSetEntry {

	private final int position;
	private final String name;
	private final Object value;

	public SingleStaticDataSetEntry(final int position, final Object value) {
		this(position, null, value);
	}

	public SingleStaticDataSetEntry(final String name, final Object value) {
		this(-1, name, value);
	}

	public SingleStaticDataSetEntry(final Object value) {
		this(-1, null, value);
	}

	public SingleStaticDataSetEntry(final int position, final String name,
			final Object value) {
		this.position = position;
		this.name = name == null ? UUID.randomUUID().toString() : name;
		this.value = value;
	}

	public int getPosition() {
		return position;
	}

	/**
	 * Gets the name assigned to {@code this} entry. The name can never be
	 * {@code null}.
	 * 
	 * @return the name of the entry, which can never be {@code null}
	 */
	public String getName() {
		return name;
	}

	public Object getValue() {
		return value;
	}

	@Override
	public String toString() {
		return value == null ? null : value.toString();
	}
}
