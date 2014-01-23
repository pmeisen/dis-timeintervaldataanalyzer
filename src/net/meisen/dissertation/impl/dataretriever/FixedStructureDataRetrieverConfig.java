package net.meisen.dissertation.impl.dataretriever;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.List;

import net.meisen.dissertation.model.dataretriever.IDataRetrieverConfig;

/**
 * A configuration which specifies the behavior of a
 * {@code FixedStructureDataRetriever}.
 * 
 * @author pmeisen
 * 
 */
public class FixedStructureDataRetrieverConfig implements IDataRetrieverConfig {

	private List<FixedStructureDataRetrieverConfigEntry> entries = new ArrayList<FixedStructureDataRetrieverConfigEntry>();

	/**
	 * Constructor which creates an empty structure.
	 */
	public FixedStructureDataRetrieverConfig() {
		this((Collection<FixedStructureDataRetrieverConfigEntry>) null);
	}

	/**
	 * Constructor to create a structure based on the specified {@code entries}.
	 * 
	 * @param entries
	 *            the entries which make up the structure
	 */
	public FixedStructureDataRetrieverConfig(
			final FixedStructureDataRetrieverConfigEntry... entries) {
		this(entries == null ? null : Arrays.asList(entries));
	}

	/**
	 * Constructor to create a structure based on the specified {@code entries}.
	 * 
	 * @param entries
	 *            the entries which make up the structure
	 */
	public FixedStructureDataRetrieverConfig(
			final Collection<FixedStructureDataRetrieverConfigEntry> entries) {
		if (entries != null) {
			addEntries(entries);
		}
	}

	/**
	 * Adds an entry to the configuration.
	 * 
	 * @param entry
	 *            the entry to be added
	 */
	public void addEntry(final FixedStructureDataRetrieverConfigEntry entry) {
		this.entries.add(entry);
	}

	/**
	 * Add several entries to the configuration.
	 * 
	 * @param entries
	 *            the entries to be added
	 */
	public void addEntries(
			final Collection<FixedStructureDataRetrieverConfigEntry> entries) {
		this.entries.addAll(entries);
	}

	/**
	 * Gets the entries of {@code this} configuration.
	 * 
	 * @return the entries of {@code this} configuration
	 */
	public List<FixedStructureDataRetrieverConfigEntry> getEntries() {
		return Collections.unmodifiableList(entries);
	}

	/**
	 * Get the names defined for the fixed structure.
	 * 
	 * @return the names defined for the fixed structure
	 */
	public String[] getNames() {
		final int size = entries.size();
		final String[] names = new String[entries.size()];
		for (int i = 0; i < size; i++) {
			names[i] = entries.get(i).getName();
		}

		return names;
	}
}
