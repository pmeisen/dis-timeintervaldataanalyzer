package net.meisen.dissertation.data.impl.dataretriever;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.List;

import net.meisen.dissertation.models.impl.dataretriever.IDataRetrieverConfig;

public class FixedStructureDataRetrieverConfig implements IDataRetrieverConfig {

	private List<FixedStructureDataRetrieverConfigEntry> entries = new ArrayList<FixedStructureDataRetrieverConfigEntry>();

	public FixedStructureDataRetrieverConfig() {
		this((Collection<FixedStructureDataRetrieverConfigEntry>) null);
	}

	public FixedStructureDataRetrieverConfig(
			final FixedStructureDataRetrieverConfigEntry... entries) {
		this(entries == null ? null : Arrays.asList(entries));
	}

	public FixedStructureDataRetrieverConfig(
			final Collection<FixedStructureDataRetrieverConfigEntry> entries) {
		if (entries != null) {
			addEntries(entries);
		}
	}

	public void addEntry(final FixedStructureDataRetrieverConfigEntry entry) {
		this.entries.add(entry);
	}

	public void addEntries(
			final Collection<FixedStructureDataRetrieverConfigEntry> entries) {
		this.entries.addAll(entries);
	}

	public List<FixedStructureDataRetrieverConfigEntry> getEntries() {
		return Collections.unmodifiableList(entries);
	}

	public String[] getNames() {
		final int size = entries.size();
		final String[] names = new String[entries.size()];
		for (int i = 0; i < size; i++) {
			names[i] = entries.get(i).getName();
		}

		return names;
	}
}
