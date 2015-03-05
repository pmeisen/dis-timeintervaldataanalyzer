package net.meisen.dissertation.model.data;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.List;

import net.meisen.dissertation.model.datastructure.IntervalStructureEntry;
import net.meisen.dissertation.model.datastructure.StructureEntry;
import net.meisen.dissertation.model.datastructure.IntervalStructureEntry.IntervalTypeFactory.IntervalType;

/**
 * A {@code DataStructure} adds the semantics and the bindings to the data. That
 * means that the retrieved data is enriched with the semantic considering the
 * type of the data.
 * 
 * @author pmeisen
 * 
 */
public class DataStructure {
	private List<StructureEntry> entryList;

	/**
	 * Constructor used to create a {@code DataStructure} with the specified
	 * {@code entries}.
	 * 
	 * @param entries
	 *            the entries to be added
	 */
	public DataStructure(final Collection<StructureEntry> entries) {
		this(entries == null ? null : entries.toArray(new StructureEntry[] {}));
	}

	/**
	 * Constructor used to create a {@code DataStructure} with the specified
	 * {@code entries}.
	 * 
	 * @param entries
	 *            the entries to be added
	 */
	public DataStructure(final StructureEntry... entries) {
		final int size = entries == null ? 0 : entries.length;

		final List<StructureEntry> entryList = new ArrayList<StructureEntry>();
		for (int i = 0; i < size; i++) {
			final StructureEntry entry = entries[i];

			// add the entry to the list
			if (entry != null) {
				entryList.add(entry);
			}
		}

		// define the entryList
		this.entryList = new ArrayList<StructureEntry>(entryList.size());
		this.entryList.addAll(entryList);
	}

	/**
	 * Get the {@code StructureEntry} instances of {@code this} of the specified
	 * {@code entryClass}. The insertion-order is thereby kept.
	 * 
	 * @param entryClass
	 *            the {@code StructureEntry}-class to get the entries for
	 * 
	 * @return the entries within the {@code DataStructure} of the specified
	 *         type
	 */
	public <T extends StructureEntry> List<T> getEntriesByClass(
			final Class<T> entryClass) {
		final List<T> entries = new ArrayList<T>();

		// get all the entries of the specified class
		for (final StructureEntry entry : entryList) {
			if (entry.getClass().equals(entryClass)) {

				@SuppressWarnings("unchecked")
				final T e = (T) entry;
				entries.add(e);
			}
		}

		return entries;
	}

	/**
	 * Gets all the entries in insertion-order.
	 * 
	 * @return the entries of the {@code DataModel}
	 */
	public List<StructureEntry> getEntries() {
		return Collections.unmodifiableList(entryList);
	}

	/**
	 * Gets the amount of entries within the {@code DataStructure}.
	 * 
	 * @return the amount of entries within the {@code DataStructure}.
	 */
	public int getSize() {
		return entryList.size();
	}

	/**
	 * Gets the defined {@code IntervalStructureEntry} for the specified
	 * {@code IntervalType}.
	 * 
	 * @param type
	 *            the type to retrieve
	 * 
	 * @return the {@code IntervalStructureEntry} of the type, or {@code null}
	 *         if non exists
	 */
	public IntervalStructureEntry getIntervalEntryOfType(final IntervalType type) {
		if (type == null) {
			return null;
		}

		final List<IntervalStructureEntry> entries = this
				.getEntriesByClass(IntervalStructureEntry.class);
		for (final IntervalStructureEntry entry : entries) {
			if (type.equals(entry.getIntervalType())) {
				return entry;
			}
		}

		return null;
	}
}
