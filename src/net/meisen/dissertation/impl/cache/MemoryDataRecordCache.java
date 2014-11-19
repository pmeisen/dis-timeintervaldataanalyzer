package net.meisen.dissertation.impl.cache;

import gnu.trove.iterator.TIntIterator;
import gnu.trove.map.hash.TIntObjectHashMap;

import java.util.Iterator;

import net.meisen.dissertation.model.cache.IDataRecordCache;
import net.meisen.dissertation.model.cache.IDataRecordCacheConfig;
import net.meisen.dissertation.model.data.TidaModel;
import net.meisen.dissertation.model.indexes.datarecord.IDataRecordMeta;
import net.meisen.dissertation.model.indexes.datarecord.ProcessedDataRecord;
import net.meisen.dissertation.model.time.mapper.BaseMapper;
import net.meisen.dissertation.model.util.IIntIterator;

/**
 * Cache which caches the records in memory only.
 * 
 * @author pmeisen
 * 
 */
public class MemoryDataRecordCache implements IDataRecordCache {

	private TIntObjectHashMap<Object[]> map = new TIntObjectHashMap<Object[]>();

	private BaseMapper<?> mapper;
	private IDataRecordMeta meta;

	@Override
	public void initialize(final TidaModel model) {

		// get the needed values
		mapper = model.getIntervalModel().getTimelineMapper();
		meta = model.getDataRecordFactory().getMeta();
	}

	@Override
	public boolean setPersistency(final boolean enable) {
		return false;
	}

	@Override
	public void release() {
		map.clear();
	}

	@Override
	public void cache(final ProcessedDataRecord record) {
		final Object[] res = record.createObjectArray(meta, mapper);

		// cache the value now
		cache(record.getId(), res);
	}

	@Override
	public void cache(final int id, final Object[] record) {
		map.put(id, record);
	}

	@Override
	public Object[] get(final int recordId) {
		return map.get(recordId);
	}

	@Override
	public void setConfig(final IDataRecordCacheConfig config)
			throws BaseIdentifierCacheException {
		// nothing to do
	}

	@Override
	public IIntIterator intIterator() {
		return new IIntIterator() {
			private final TIntIterator it = map.keySet().iterator();

			@Override
			public boolean hasNext() {
				return it == null ? false : it.hasNext();
			}

			@Override
			public int next() {
				return it.next();
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
		return map.size();
	}

	@Override
	public void remove() {
		// nothing to do
	}
}
