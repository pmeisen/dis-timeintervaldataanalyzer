package net.meisen.dissertation.impl.datasets;

import java.util.Arrays;
import java.util.Collection;
import java.util.HashSet;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;

import net.meisen.dissertation.model.datasets.IDataRecord;
import net.meisen.dissertation.model.datasets.IDataSet;

/**
 * An implementation of a {@code DataSet} with just one single
 * {@code DataRecord}. The {@code DataSet} is defined by it's
 * {@code SingleStaticDataSetEntries}.
 * 
 * @author pmeisen
 * 
 * @see SingleStaticDataSetEntry
 * 
 */
public class SingleStaticDataSet implements IDataSet, IDataRecord {

	private final SingleStaticDataSetEntry[] entries;

	/**
	 * Constructor to create a {@code SingleStaticDataSet} for the specified
	 * {@code values}. The entries will be generated for each value and
	 * positioned by insertion-order.
	 * 
	 * @param values
	 *            the values of the {@code SingleStaticDataSet}
	 */
	public SingleStaticDataSet(final Object... values) {

		if (values == null) {
			this.entries = new SingleStaticDataSetEntry[0];
		} else {
			this.entries = new SingleStaticDataSetEntry[values.length];
			for (int i = 0; i < values.length; i++) {
				final Object object = values[i];
				if (object != null) {
					this.entries[i] = new SingleStaticDataSetEntry(i, object);
				}
			}
		}
	}

	/**
	 * Constructor to create a {@code SingleStaticDataSet} for the specified
	 * {@code map}.
	 * 
	 * @param map
	 *            the map with the data of the dataset.
	 */
	public SingleStaticDataSet(final Map<String, Object> map) {

		if (map == null) {
			this.entries = new SingleStaticDataSetEntry[0];
		} else {
			this.entries = new SingleStaticDataSetEntry[map.size()];

			int i = 0;
			for (final Entry<String, Object> entry : map.entrySet()) {
				if (entry != null) {
					this.entries[i] = new SingleStaticDataSetEntry(i + 1,
							entry.getKey(), entry.getValue());
				}
				i++;
			}
		}
	}

	/**
	 * Constructor to create a {@code SingleStaticDataSet} for the specified
	 * {@code entries}.
	 * 
	 * @param entries
	 *            the entries of the {@code SingleStaticDataSet}
	 */
	public SingleStaticDataSet(
			final Collection<SingleStaticDataSetEntry> entries) {
		this(entries == null ? null : entries
				.toArray(new SingleStaticDataSetEntry[] {}));
	}

	/**
	 * Constructor to create a {@code SingleStaticDataSet} for the specified
	 * {@code entries}.
	 * 
	 * @param entries
	 *            the entries of the {@code SingleStaticDataSet}
	 */
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

		// make sure there are no names duplicates
		final Set<String> names = new HashSet<String>();

		// set all the dataSetEntries
		int curPos = -1;
		this.entries = new SingleStaticDataSetEntry[initCapacity];
		for (final SingleStaticDataSetEntry entry : entries) {
			curPos++;

			if (entry == null) {
				continue;
			}

			// check if the position can be used
			final int pos = entry.getPosition() < 1 ? curPos : entry
					.getPosition() - 1;
			if (this.entries[pos] == null) {
				this.entries[pos] = entry;
			} else {
				throw new IllegalArgumentException("The position '" + (pos + 1)
						+ "' is used by multiple entries.");
			}

			// check the name
			if (!names.add(entry.getName())) {
				throw new IllegalArgumentException("The name '"
						+ entry.getName() + "' is used multiple times.");
			}
		}
	}

	@Override
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

	@Override
	public Object getValue(final int position) {
		if (!isValidPosition(position)) {
			throw new IllegalArgumentException("The position '" + position
					+ "' is invalid.");
		} else {
			return getValue(entries[position - 1]);
		}
	}

	@Override
	public boolean hasNamedValue(final String name) {
		for (final SingleStaticDataSetEntry entry : entries) {
			if (entry == null) {
				continue;
			} else if (name == null) {
				return false;
			} else if (name.equals(entry.getName())) {
				return true;
			}
		}

		return false;
	}

	@Override
	public boolean isValidPosition(final int position) {
		return !(position < 1 || position > entries.length);
	}

	/**
	 * Gets the value of the entry.
	 * 
	 * @param entry
	 *            the entry to get the value for
	 * 
	 * @return the value of the entry
	 */
	protected Object getValue(final SingleStaticDataSetEntry entry) {
		if (entry == null) {
			return null;
		} else {
			return entry.getValue();
		}
	}

	@Override
	public SingleStaticDataSetIterator iterator() {
		return new SingleStaticDataSetIterator(this);
	}

	@Override
	public boolean isOfflineAvailable() {
		return true;
	}

	@Override
	public String toString() {
		return Arrays.asList(entries).toString();
	}

	@Override
	public String getName(final int pos) {
		if (!isValidPosition(pos)) {
			throw new IllegalArgumentException("The position '" + pos
					+ "' is invalid.");
		}

		return entries[pos - 1].getName();
	}

	@Override
	public int getPosition(final String name) {

		int pos = 0;
		for (final SingleStaticDataSetEntry entry : entries) {
			pos++;

			if (entry.getName().equals(name)) {
				return pos;
			}
		}

		return 0;
	}
}
