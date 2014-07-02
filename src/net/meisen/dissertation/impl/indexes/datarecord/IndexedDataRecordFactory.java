package net.meisen.dissertation.impl.indexes.datarecord;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;

import net.meisen.dissertation.config.xslt.DefaultValues;
import net.meisen.dissertation.exceptions.IndexedDataRecordFactoryException;
import net.meisen.dissertation.model.data.TidaModel;
import net.meisen.dissertation.model.indexes.datarecord.IDataRecordFactory;
import net.meisen.general.genmisc.exceptions.registry.IExceptionRegistry;

/**
 * Factory to create {@code DataRecord} instances from the cache i.e. an object
 * array.
 * 
 * @author pmeisen
 * 
 */
public class IndexedDataRecordFactory implements IDataRecordFactory {

	@Autowired
	@Qualifier(DefaultValues.EXCEPTIONREGISTRY_ID)
	private IExceptionRegistry exceptionRegistry;

	private IndexedDataRecordMeta meta;

	@Override
	public void initialize(final TidaModel model) {
		this.meta = new IndexedDataRecordMeta(model);
	}

	@Override
	public IndexedDataRecordMeta getMeta() {
		if (meta == null) {
			exceptionRegistry.throwException(
					IndexedDataRecordFactoryException.class, 1000);
		}
		return meta;
	}

	@Override
	public IndexedDataRecord create(final Object[] values) {
		if (meta == null) {
			exceptionRegistry.throwException(
					IndexedDataRecordFactoryException.class, 1000);
		} else if (values == null) {
			exceptionRegistry.throwException(
					IndexedDataRecordFactoryException.class, 1001);
		}
		return new IndexedDataRecord(meta, values);
	}
}
