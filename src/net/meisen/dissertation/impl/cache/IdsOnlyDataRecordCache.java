package net.meisen.dissertation.impl.cache;

import java.util.Iterator;

import net.meisen.dissertation.model.cache.IDataRecordCache;
import net.meisen.dissertation.model.cache.IDataRecordCacheConfig;
import net.meisen.dissertation.model.data.FieldNameGenerator;
import net.meisen.dissertation.model.data.TidaModel;
import net.meisen.dissertation.model.indexes.datarecord.ProcessedDataRecord;
import net.meisen.dissertation.model.util.IIntIterator;

/**
 * Cache which only caches the record identifiers, really it does not cache
 * anything, because the identifier is the reference.
 * 
 * @author pmeisen
 * 
 */
public class IdsOnlyDataRecordCache implements IDataRecordCache {

	@Override
	public boolean setPersistency(final boolean enable) {
		return false;
	}

	@Override
	public void release() {
		// nothing to do
	}

	@Override
	public void initialize(final TidaModel model) {
		// nothing to do
	}

	@Override
	public void cache(final ProcessedDataRecord record) {
		// nothing to do
	}
	
	@Override
	public void cache(final int id, final Object[] record) {
		// nothing to do
	}

	@Override
	public Object[] get(final int recordId) {
		return new Object[] { recordId };
	}

	@Override
	public String[] getNames() {
		return new String[] { FieldNameGenerator.get().getIdFieldName() };
	}

	@Override
	public Class<?>[] getTypes() {
		return new Class<?>[] { int.class };
	}

	@Override
	public void setConfig(final IDataRecordCacheConfig config)
			throws BaseIdentifierCacheException {
		// nothing to do
	}

	@Override
	public IIntIterator intIterator() {
		return new IIntIterator() {

			@Override
			public boolean hasNext() {
				return false;
			}

			@Override
			public int next() {
				throw new IllegalStateException("There is no next element.");
			}
		};
	}

	/**
	 * Consider to use {@link #intIterator()} instead of {@code this}.
	 */
	@Override
	public Iterator<Integer> iterator() {
		return new Iterator<Integer>() {
			private final IIntIterator it = intIterator();

			@Override
			public boolean hasNext() {
				return it == null ? false : it.hasNext();
			}

			@Override
			public Integer next() {
				return it.next();
			}

			@Override
			public void remove() {
				throw new UnsupportedOperationException(
						"Remove is not supported.");
			}
		};
	}

	@Override
	public int size() {
		return 0;
	}
}
