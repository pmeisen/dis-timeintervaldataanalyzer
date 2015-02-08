package net.meisen.dissertation.impl.indexes.datarecord;

import java.util.Arrays;
import java.util.Collection;

import net.meisen.dissertation.jdbc.protocol.DataType;
import net.meisen.dissertation.model.data.FieldNameGenerator;
import net.meisen.dissertation.model.data.MetaDataModel;
import net.meisen.dissertation.model.data.TidaModel;
import net.meisen.dissertation.model.descriptors.DescriptorModel;
import net.meisen.dissertation.model.indexes.datarecord.IDataRecordMeta;
import net.meisen.dissertation.model.time.mapper.BaseMapper;

/**
 * Meta information of a {@code IndexedDataRecord}.
 * 
 * @author pmeisen
 * 
 * @see IndexedDataRecord
 * 
 */
public class IndexedDataRecordMeta implements IDataRecordMeta {

	private final String[] names;
	private final Class<?>[] types;
	private final DataType[] dataTypes;
	private final String[] descModelIds;

	private final int posRecordId;
	private final int posStart;
	private final int posEnd;
	private final int posOffsetDescModelIds;

	/**
	 * Constructor creating the meta-information based on the defined
	 * {@code model}.
	 * 
	 * @param model
	 *            the model to create the meta instance for
	 */
	public IndexedDataRecordMeta(final TidaModel model) {
		final BaseMapper<?> mapper = model.getIntervalModel()
				.getTimelineMapper();

		final Class<?> intervalType = mapper.getMappedType();
		final MetaDataModel metaDataModel = model.getMetaDataModel();
		final Collection<DescriptorModel<?>> descModels = metaDataModel
				.getDescriptorModels();

		// id + interval + descriptors
		final int descModelsSize = descModels.size();
		final int size = 1 + 2 + descModelsSize;
		names = new String[size];
		types = new Class<?>[size];
		dataTypes = new DataType[size];
		descModelIds = new String[descModelsSize];

		int currentPos = 0;

		final FieldNameGenerator fg = FieldNameGenerator.get();
		names[currentPos] = fg.getIdFieldName();
		dataTypes[currentPos] = DataType.find(int.class);
		types[currentPos] = dataTypes[currentPos].getRepresentorClass();
		posRecordId = currentPos + 1;
		currentPos++;

		names[currentPos] = fg.getIntervalStartFieldName();
		dataTypes[currentPos] = DataType.find(intervalType);
		types[currentPos] = dataTypes[currentPos].getRepresentorClass();
		posStart = currentPos + 1;
		currentPos++;

		names[currentPos] = fg.getIntervalEndFieldName();
		dataTypes[currentPos] = DataType.find(intervalType);
		types[currentPos] = dataTypes[currentPos].getRepresentorClass();
		posEnd = currentPos + 1;
		currentPos++;

		/*
		 * Next come the descriptors
		 */
		posOffsetDescModelIds = currentPos + 1;
		for (final DescriptorModel<?> descModel : descModels) {
			descModelIds[currentPos - (posOffsetDescModelIds - 1)] = descModel
					.getId();

			names[currentPos] = descModel.getName();
			dataTypes[currentPos] = DataType.find(descModel.getValueType());
			if (dataTypes[currentPos] == null) {
				dataTypes[currentPos] = DataType.STRING;
			}
			types[currentPos] = dataTypes[currentPos].getRepresentorClass();

			currentPos++;
		}
	}

	@Override
	public DataType[] getDataTypes() {
		return dataTypes;
	}

	@Override
	public String getDescriptorModelId(final int position) {

		final int pos = position - posOffsetDescModelIds;
		if (pos < 0) {
			return null;
		} else if (pos > getLastPosDescModelIds()) {
			return null;
		} else {
			return descModelIds[position - posOffsetDescModelIds];
		}
	}

	@Override
	public Class<?>[] getTypes() {
		return types;
	}

	@Override
	public String[] getNames() {
		return names;
	}

	@Override
	public int getPosRecordId() {
		return posRecordId;
	}

	@Override
	public int getPosStart() {
		return posStart;
	}

	@Override
	public int getPosEnd() {
		return posEnd;
	}

	@Override
	public int getFirstPosDescModelIds() {
		return posOffsetDescModelIds;
	}

	@Override
	public int sizeOfDescModelIds() {
		return descModelIds.length;
	}

	/**
	 * Gets the position (1-based) of the field with the specified {@code name}.
	 * The name is the real used named of the field (i.e. not the
	 * descriptorModel's identifier).
	 * 
	 * @param name
	 *            the name of the field to retrieve the position for
	 * 
	 * @return the position (1-based) or {@code -1} if the position could not be
	 *         found
	 * 
	 * @see #getPosition(String, boolean)
	 */
	public int getPosition(final String name) {
		return getPosition(name, false);
	}

	/**
	 * Gets the position (1-based) of the field with the specified {@code name}.
	 * The position of a field representing a descriptor's value can be
	 * retrieved by the real name or the descriptorModel's identifier (see
	 * {@code useDescriptorIdAsName}).
	 * 
	 * @param name
	 *            the name of the field to retrieve the position for
	 * @param useDescriptorIdAsName
	 *            {@code true} if the name should be matched against a
	 *            descriptorModel's identifier, or {@code false} if the name
	 *            should be matched against the real name
	 * 
	 * @return the position (1-based) or {@code -1} if the position could not be
	 *         found
	 * 
	 * @see #getPosition(String, boolean)
	 */
	public int getPosition(final String name,
			final boolean useDescriptorIdAsName) {

		if (name == null) {
			return -1;
		} else if (useDescriptorIdAsName) {

			// check the descriptorModels
			for (int i = 0; i < descModelIds.length; i++) {
				final String descModelId = descModelIds[i];

				if (name.equals(descModelId)) {
					return getFirstPosDescModelIds() + i;
				}
			}

			// determine the names of recId and interval
			if (names[getPosRecordId() - 1].equals(name)) {
				return getPosRecordId();
			} else if (names[getPosStart() - 1].equals(name)) {
				return getPosStart();
			} else if (names[getPosEnd() - 1].equals(name)) {
				return getPosEnd();
			} else {
				return -1;
			}
		} else {
			for (int i = 0; i < names.length; i++) {
				final String fieldName = names[i];
				if (name.equals(fieldName)) {
					return i;
				}
			}

			return -1;
		}
	}

	@Override
	public int getLastPosDescModelIds() {
		return getFirstPosDescModelIds() + sizeOfDescModelIds() - 1;
	}

	@Override
	public String toString() {
		return "Names: " + Arrays.asList(names).toString() + ", Types: "
				+ Arrays.asList(dataTypes).toString() + ", DescriptorModels: "
				+ Arrays.asList(descModelIds).toString();
	}
}
