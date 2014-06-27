package net.meisen.dissertation.model.indexes.datarecord;

import java.io.InputStream;

import net.meisen.dissertation.model.cache.IDataRecordCache;
import net.meisen.dissertation.model.data.TidaModel;
import net.meisen.dissertation.model.persistence.BasePersistor;
import net.meisen.dissertation.model.persistence.Group;
import net.meisen.dissertation.model.persistence.Identifier;
import net.meisen.general.genmisc.exceptions.ForwardedRuntimeException;

public class DataRecordIndex implements IDataRecordIndex {
	private final IDataRecordCache recordCache;

	public DataRecordIndex(final TidaModel model) {
		this(model.getDataRecordCache());
	}

	public DataRecordIndex(final IDataRecordCache recordCache) {
		this.recordCache = recordCache;
	}

	@Override
	public void index(final ProcessedDataRecord record) {
		this.recordCache.cache(record);
	}

	public String[] getNames() {
		return this.recordCache.getNames();
	}

	public Class<?>[] getTypes() {
		return this.recordCache.getTypes();
	}

	public Object[] get(final int recordId) {
		return this.recordCache.get(recordId);
	}

	@Override
	public void optimize() {
		// nothing to be optimized
	}

	@Override
	public void isRegistered(final BasePersistor persistor, final Group group) {
		// TODO Auto-generated method stub
	}

	@Override
	public void save(final BasePersistor persistor)
			throws ForwardedRuntimeException {
		// TODO Auto-generated method stub
	}

	@Override
	public void load(final BasePersistor persistor,
			final Identifier identifier, final InputStream inputStream)
			throws ForwardedRuntimeException {
		// TODO Auto-generated method stub
	}

	@Override
	public Group getPersistentGroup() {
		// TODO Auto-generated method stub
		return null;
	}
}
