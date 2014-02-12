package net.meisen.dissertation.model.indexes.datarecord;

import net.meisen.dissertation.model.datastructure.IntervalStructureEntry;
import net.meisen.dissertation.model.indexes.BaseIndexedCollectionFactory;

public class IntervalIndexDimension {
	
	private final IntervalStructureEntry startEntry;
	private final IntervalStructureEntry endEntry;
	
	private BaseIndexedCollectionFactory indexedCollectionFactory;

	public IntervalIndexDimension(final IntervalStructureEntry start,
			final IntervalStructureEntry end,
			final BaseIndexedCollectionFactory indexedCollectionFactory) {
		this.startEntry = start;
		this.endEntry = end;
		
		this.indexedCollectionFactory = indexedCollectionFactory;
	}

	
}
