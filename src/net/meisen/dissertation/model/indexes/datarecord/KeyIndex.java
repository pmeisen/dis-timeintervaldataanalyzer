package net.meisen.dissertation.model.indexes.datarecord;

import java.util.List;

import net.meisen.dissertation.model.data.DataStructure;
import net.meisen.dissertation.model.data.TidaModel;
import net.meisen.dissertation.model.datasets.IDataRecord;
import net.meisen.dissertation.model.datastructure.KeyStructureEntry;

public class KeyIndex implements DataRecordIndex {
	private final List<KeyIndexDimension> keyDimensions;

	public KeyIndex(final TidaModel model) {

		// get the DataStructure which defines how the index has to look like
		final DataStructure dataStructure = model.getDataStructure();

		final List<KeyStructureEntry> entries = dataStructure
				.getEntriesByClass(KeyStructureEntry.class);

		keyDimensions = null;
	}

	@Override
	public void index(final int dataId, final IDataRecord record) {
		// TODO Auto-generated method stub
	}

	@Override
	public void optimize() {
		// TODO Auto-generated method stub
		
	}
}
