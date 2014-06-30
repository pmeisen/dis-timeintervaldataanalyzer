package net.meisen.dissertation.impl.cache;

import gnu.trove.iterator.TIntIterator;
import gnu.trove.map.hash.TIntObjectHashMap;

import java.util.Collection;
import java.util.Iterator;

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
 * Cache which caches the records in memory only.
 * 
 * @author pmeisen
 * 
 */
public class MemoryDataRecordCache implements IDataRecordCache {

	private BaseMapper<?> mapper;

	private String[] descModelIds;
	private String[] names;
	private Class<?>[] types;
	private DataType[] dataTypes;

	private TIntObjectHashMap<Object[]> map = new TIntObjectHashMap<Object[]>();

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
	public String[] getNames() {
		return names;
	}

	@Override
	public Class<?>[] getTypes() {
		return types;
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
}
