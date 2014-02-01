package net.meisen.dissertation.model.data;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

import net.meisen.dissertation.config.xslt.DefaultValues;
import net.meisen.dissertation.model.datastructure.StructureEntry;
import net.meisen.general.genmisc.exceptions.registry.IExceptionRegistry;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;

public class DataStructure {

	@Autowired
	@Qualifier(DefaultValues.EXCEPTIONREGISTRY_ID)
	private IExceptionRegistry exceptionRegistry;

	private List<StructureEntry> entryList;

	public DataStructure(final Collection<StructureEntry> entries) {
		this(entries == null ? null : entries.toArray(new StructureEntry[] {}));
	}

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

	public int getSize() {
		return entryList.size();
	}
}
