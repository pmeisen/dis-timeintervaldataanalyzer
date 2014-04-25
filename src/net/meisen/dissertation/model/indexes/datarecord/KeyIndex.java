package net.meisen.dissertation.model.indexes.datarecord;

import java.io.InputStream;
import java.util.List;

import net.meisen.dissertation.model.data.DataStructure;
import net.meisen.dissertation.model.data.TidaModel;
import net.meisen.dissertation.model.datasets.IDataRecord;
import net.meisen.dissertation.model.datastructure.KeyStructureEntry;
import net.meisen.dissertation.model.persistence.BasePersistor;
import net.meisen.dissertation.model.persistence.Group;
import net.meisen.dissertation.model.persistence.Identifier;

public class KeyIndex implements IDataRecordIndex {
	private final List<KeyIndexDimension> keyDimensions;

	private Group persistentGroup = null;

	public KeyIndex(final TidaModel model) {

		// get the DataStructure which defines how the index has to look like
		final DataStructure dataStructure = model.getDataStructure();

		final List<KeyStructureEntry> entries = dataStructure
				.getEntriesByClass(KeyStructureEntry.class);

		keyDimensions = null;
	}

	@Override
	public void index(final ProcessedDataRecord record) {
		// TODO Auto-generated method stub
	}

	@Override
	public void optimize() {
		// TODO Auto-generated method stub

	}

	@Override
	public void save(final BasePersistor basePersistor) {
		// TODO Auto-generated method stub
	}

	@Override
	public void load(final BasePersistor persistor,
			final Identifier identifier, final InputStream inputStream) {
		// TODO Auto-generated method stub
	}

	@Override
	public void isRegistered(final BasePersistor persistor, final Group group) {
		// TODO Auto-generated method stub
	}

	@Override
	public Group getPersistentGroup() {
		return persistentGroup;
	}
}
