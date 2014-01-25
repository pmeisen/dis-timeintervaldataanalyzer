package net.meisen.dissertation.model.datasets;

import java.util.Collection;

public class SingleStaticDataSet extends BaseDataSet {
	private final SingleStaticDataSetEntry[] entries;

	public SingleStaticDataSet(final Object... objects) {

		if (objects == null) {
			this.entries = new SingleStaticDataSetEntry[0];
		} else {
			this.entries = new SingleStaticDataSetEntry[objects.length];
			for (int i = 0; i < objects.length; i++) {
				final Object object = objects[i];
				if (object != null) {
					this.entries[i] = new SingleStaticDataSetEntry(i, object);
				}
			}
		}
	}

	public SingleStaticDataSet(final SingleStaticDataSetEntry... entries) {
		final int initCapacity;

		// create the indexes to access the data
		if (entries == null) {
			initCapacity = 0;
		} else {
			int max = -1;
			for (final SingleStaticDataSetEntry entry : entries) {
				if (entry == null) {
					continue;
				}

				final int position = entry.getPosition();
				if (max < position) {
					max = position;
				}
			}

			initCapacity = Math.max(max, entries.length);
		}

		this.entries = new SingleStaticDataSetEntry[initCapacity];
		int curPos = -1;
		for (final SingleStaticDataSetEntry entry : entries) {
			curPos++;

			if (entry == null) {
				continue;
			}

			final int pos = entry.getPosition() < 1 ? curPos : entry
					.getPosition() - 1;
			if (this.entries[pos] == null) {
				this.entries[pos] = entry;
			} else {
				throw new IllegalArgumentException("The position '" + (pos + 1)
						+ "' is used by multiple entries.");
			}
		}
	}

	public SingleStaticDataSet(
			final Collection<SingleStaticDataSetEntry> entries) {
		this(entries == null ? null : entries
				.toArray(new SingleStaticDataSetEntry[] {}));
	}

	public Object getValue(final String name) {
		if (name == null) {
			throw new NullPointerException("The name cannot be null.");
		}

		for (final SingleStaticDataSetEntry entry : entries) {
			if (entry == null) {
				continue;
			} else if (name.equals(entry.getName())) {
				return getValue(entry);
			}
		}

		throw new IllegalArgumentException("The name '" + name
				+ "' is not available within the dataset.");
	}

	public Object getValue(final int position) {
		if (position < 1 || position > entrySize()) {
			throw new IllegalArgumentException("The position '" + position
					+ "' is invalid.");
		} else {
			return getValue(entries[position - 1]);
		}
	}

	public int size() {
		return 1;
	}
	
	public int entrySize() {
		return entries.length;
	}

	protected Object getValue(final SingleStaticDataSetEntry entry) {
		if (entry == null) {
			return null;
		} else {
			return entry.getValue();
		}
	}
}
