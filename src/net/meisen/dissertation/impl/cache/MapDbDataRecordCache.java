package net.meisen.dissertation.impl.cache;

import java.util.Collection;
import java.util.Iterator;

import org.mapdb.Serializer;

import net.meisen.dissertation.jdbc.protocol.DataType;
import net.meisen.dissertation.model.cache.IDataRecordCache;
import net.meisen.dissertation.model.cache.IDataRecordCacheConfig;
import net.meisen.dissertation.model.data.FieldNameGenerator;
import net.meisen.dissertation.model.data.MetaDataModel;
import net.meisen.dissertation.model.data.TidaModel;
import net.meisen.dissertation.model.descriptors.Descriptor;
import net.meisen.dissertation.model.descriptors.DescriptorModel;
import net.meisen.dissertation.model.indexes.datarecord.ProcessedDataRecord;
import net.meisen.dissertation.model.time.mapper.BaseMapper;
import net.meisen.dissertation.model.util.IIntIterator;

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

	private String[] descModelIds;
	private String[] names;
	private Class<?>[] types;
	private DataType[] dataTypes;

	@Override
	public void initialize(final TidaModel model) {

		// get the needed values
		mapper = model.getIntervalModel().getTimelineMapper();

		final Class<?> intervalType = mapper.getMappedType();
		final MetaDataModel metaDataModel = model.getMetaDataModel();
		final Collection<DescriptorModel<?>> descModels = metaDataModel
				.getDescriptorModels();

		// id + interval + descriptors
		final int size = 1 + 2 + descModels.size();
		names = new String[size];
		types = new Class<?>[size];
		dataTypes = new DataType[size];
		descModelIds = new String[descModels.size()];

		final FieldNameGenerator fg = FieldNameGenerator.get();
		names[0] = fg.getIdFieldName();
		dataTypes[0] = DataType.find(int.class);
		types[0] = dataTypes[0].getRepresentorClass();

		names[1] = fg.getIntervalStartFieldName();
		dataTypes[1] = DataType.find(intervalType);
		types[1] = dataTypes[1].getRepresentorClass();

		names[2] = fg.getIntervalEndFieldName();
		dataTypes[2] = DataType.find(intervalType);
		types[2] = dataTypes[2].getRepresentorClass();

		// create the record types
		int pos = 3;
		for (final DescriptorModel<?> descModel : descModels) {
			descModelIds[pos - 3] = descModel.getId();

			names[pos] = descModel.getName();
			dataTypes[pos] = DataType.find(descModel.getValueType());
			if (dataTypes[pos] == null) {
				dataTypes[pos] = DataType.STRING;
			}
			types[pos] = dataTypes[pos].getRepresentorClass();

			pos++;
		}

		// now initialize the real cache, so that the types and names are known
		super.initialize(model);
	}

	@Override
	public String[] getNames() {
		return names;
	}

	@Override
	public Class<?>[] getTypes() {
		return types;
	}

	/**
	 * Gets the used {@code DataTypes} of the different values of the record.
	 * 
	 * @return the used {@code DataTypes} of the different values of the record
	 */
	public DataType[] getDataTypes() {
		return dataTypes;
	}

	@Override
	public void cache(final ProcessedDataRecord record) {
		final Object[] res = new Object[names.length];

		// get the values
		res[0] = record.getId();
		res[1] = mapper.resolve(record.getStart());
		res[2] = mapper.resolve(record.getEnd());

		for (int pos = 3; pos < names.length; pos++) {
			final Descriptor<?, ?, ?> desc = record
					.getDescriptor(descModelIds[pos - 3]);

			// if we have a string use the uniqueString
			if (DataType.STRING.equals(dataTypes[pos])) {
				res[pos] = desc.getUniqueString();
			} else {
				res[pos] = desc.getValue();
			}
		}

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
