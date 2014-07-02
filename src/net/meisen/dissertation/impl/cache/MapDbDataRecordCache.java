package net.meisen.dissertation.impl.cache;

import java.util.Iterator;

import net.meisen.dissertation.jdbc.protocol.DataType;
import net.meisen.dissertation.model.cache.IDataRecordCache;
import net.meisen.dissertation.model.cache.IDataRecordCacheConfig;
import net.meisen.dissertation.model.data.TidaModel;
import net.meisen.dissertation.model.indexes.datarecord.IDataRecordMeta;
import net.meisen.dissertation.model.indexes.datarecord.ProcessedDataRecord;
import net.meisen.dissertation.model.time.mapper.BaseMapper;
import net.meisen.dissertation.model.util.IIntIterator;

import org.mapdb.Serializer;

/**
 * A cache based on the {@code mapDb} implementation, used to cache the
 * different records.
 * 
 * @author pmeisen
 * 
 */
public class MapDbDataRecordCache extends BaseMapDbCache<Integer, Object[]>
		implements IDataRecordCache {

	private BaseMapper<?> mapper;
	private IDataRecordMeta meta;

	@Override
	public void initialize(final TidaModel model) {

		// get the needed values
		this.mapper = model.getIntervalModel().getTimelineMapper();
		this.meta = model.getDataRecordFactory().getMeta();
		
		// now initialize the real cache, so that the types and names are known
		super.initialize(model);
	}

	/**
	 * Gets the used {@code DataTypes} of the different values of the record.
	 * 
	 * @return the used {@code DataTypes} of the different values of the record
	 */
	public DataType[] getDataTypes() {
		return meta.getDataTypes();
	}

	@Override
	public void cache(final ProcessedDataRecord record) {
		final Object[] res = record.createObjectArray(meta, mapper); 

		// cache the value now
		super.cache(record.getId(), res);
	}

	@Override
	public void cache(final int id, final Object[] record) {
		super.cache(id, record);
	}

	@Override
	public Object[] get(int recordId) {
		return super.get(recordId);
	}

	@Override
	protected MapDbType getDefaultType() {
		return MapDbType.BTree;
	}

	@Override
	protected Object[] createNewInstance() {
		return null;
	}

	@Override
	protected MapDbDataRecordSerializer createValueSerializer() {
		return new MapDbDataRecordSerializer();
	}

	@Override
	protected String getIndexFileName() {
		return "mapDbRecord.idx";
	}

	@Override
	public void setConfig(final IDataRecordCacheConfig config) {
		if (config == null || config instanceof MapDbCacheConfig) {
			super.setConfig((MapDbCacheConfig) config);
		} else {
			exceptionRegistry.throwException(BaseMapDbCacheException.class,
					1001, config.getClass().getName());
		}
	}

	@Override
	protected Serializer<Integer> createKeySerializer() {
		return null;
	}

	@Override
	public IIntIterator intIterator() {
		return new IIntIterator() {
			private final Iterator<Integer> it = keyIterator();

			@Override
			public int next() {
				return it.next();
			}

			@Override
			public boolean hasNext() {
				return it.hasNext();
			}
		};
	}

	@Override
	public Iterator<Integer> iterator() {
		return keyIterator();
	}

	@Override
	public int size() {
		return super.size();
	}
}
