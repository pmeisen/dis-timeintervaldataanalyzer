package net.meisen.dissertation.model.indexes.datarecord;

import java.io.InputStream;

import net.meisen.dissertation.model.cache.IDataRecordCache;
import net.meisen.dissertation.model.data.FieldNameGenerator;
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

		/*
		 * We don't use any additional data-structure for indexing, just the
		 * cache.
		 */
		if (isEnabled()) {
			this.recordCache.cache(record);
		}
	}

	public String[] getNames() {
		if (isEnabled()) {
			return this.recordCache.getNames();
		} else {
			return new String[] { FieldNameGenerator.get().getIdFieldName() };
		}
	}

	public Class<?>[] getTypes() {
		if (isEnabled()) {
			return this.recordCache.getTypes();
		} else {
			return new Class<?>[] { int.class };
		}
	}

	public Object[] get(final int recordId) {
		if (isEnabled()) {
			return this.recordCache.get(recordId);
		} else {
			return new Object[] { recordId };
		}
	}

	public boolean isEnabled() {
		return this.recordCache != null;
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
