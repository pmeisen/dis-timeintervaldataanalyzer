package net.meisen.dissertation.model.indexes.datarecord;

import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.util.HashMap;
import java.util.Map;

import net.meisen.dissertation.exceptions.PersistorException;
import net.meisen.dissertation.exceptions.TidaIndexException;
import net.meisen.dissertation.model.data.DataStructure;
import net.meisen.dissertation.model.data.TidaModel;
import net.meisen.dissertation.model.datasets.IDataRecord;
import net.meisen.dissertation.model.descriptors.Descriptor;
import net.meisen.dissertation.model.descriptors.FactDescriptor;
import net.meisen.dissertation.model.indexes.BaseIndexFactory;
import net.meisen.dissertation.model.indexes.datarecord.slices.Bitmap;
import net.meisen.dissertation.model.indexes.datarecord.slices.Slice;
import net.meisen.dissertation.model.indexes.datarecord.slices.SliceWithDescriptors;
import net.meisen.dissertation.model.persistence.BasePersistor;
import net.meisen.dissertation.model.persistence.Group;
import net.meisen.dissertation.model.persistence.IPersistable;
import net.meisen.dissertation.model.persistence.Identifier;
import net.meisen.general.genmisc.exceptions.ForwardedRuntimeException;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * An index for {@code TimeIntervalDataAnalysis}. The index is used to select
 * {@code DataRecords} efficient considering the querying time.
 * 
 * @author pmeisen
 * 
 */
public class TidaIndex implements IPersistable {
	private final static String EXTENSION = ".config";
	private final static Logger LOG = LoggerFactory.getLogger(TidaIndex.class);

	private final int lastValidId;
	private final TidaModel model;

	private final Map<Class<? extends IDataRecordIndex>, IDataRecordIndex> indexes;
	private final IntervalIndex intervalIndex;
	private final MetaIndex metaIndex;
	private final DataRecordIndex recordIndex;

	private int dataId;
	private Group persistentGroup = null;

	/**
	 * Creates an index for the passed {@code model}. The first record will be
	 * identifier with the first valid identifier available by the underlying
	 * bitmap implementation (see {@link Bitmap#getMinId()}.
	 * 
	 * @param model
	 *            the {@code TidaModel} to be added
	 * 
	 * @see TidaModel
	 */
	public TidaIndex(final TidaModel model) {
		this(model, -1);
	}

	/**
	 * Creates an index for the passed {@code model}.
	 * 
	 * @param model
	 *            the {@code TidaModel} to be added
	 * @param lastUsedId
	 *            the last used identifier, i.e. the index will index new
	 *            records with an identifier larger than the last used one. Pass
	 *            {@code -1} to initialize the index with the first valid
	 *            identifier.
	 * 
	 * @see TidaModel
	 */
	public TidaIndex(final TidaModel model, final int lastUsedId) {
		this.model = model;
		this.indexes = new HashMap<Class<? extends IDataRecordIndex>, IDataRecordIndex>();

		// determine the last valid identifier
		final Bitmap bmp = model.getIndexFactory().createBitmap();
		this.lastValidId = Math.min(Integer.MAX_VALUE - 1, bmp.getMaxId());

		if (lastUsedId < 0) {
			this.dataId = Math.max(0, bmp.getMinId());
			if (this.dataId != 0 && LOG.isWarnEnabled()) {
				LOG.warn("The minimal identifier is defined to be '"
						+ this.dataId
						+ "'. It is not suggested to use a different minimal value than '0'.");
			}
		} else {
			this.dataId = lastUsedId + 1;
		}

		// create the indexes
		intervalIndex = new IntervalIndex(model);
		metaIndex = new MetaIndex(model);
		recordIndex = new DataRecordIndex(model);

		// add the dimensions of the MetaDataModel, Key and Interval
		indexes.put(MetaIndex.class, metaIndex);
		indexes.put(IntervalIndex.class, intervalIndex);
		indexes.put(DataRecordIndex.class, recordIndex);
	}

	/**
	 * Get the {@code IndexFactory} used by the model.
	 * 
	 * @return the {@code IndexFactory} used by the model
	 */
	public BaseIndexFactory getIndexFactory() {
		return model.getIndexFactory();
	}

	/**
	 * Gets the record of the specified {@code recordId}.
	 * 
	 * @param recordId
	 *            the identifier of the record to retrieve the values for
	 * 
	 * @return the values of the record, {@code null} if the identifier is
	 *         invalid
	 */
	public IDataRecord getRecord(final int recordId) {
		return recordIndex.get(recordId);
	}

	/**
	 * Gets the object-array representing the values of the record with the
	 * specified {@code recordId}.
	 * 
	 * @param recordId
	 *            the identifier of the record for
	 * 
	 * @return the object-array representing the values of the record with the
	 *         specified {@code recordId}; {@code null} if no record with the
	 *         specified identifier exists
	 */
	public Object[] getRecordAsArray(final int recordId) {
		return recordIndex.getArray(recordId);
	}

	/**
	 * Gets the types of record values.
	 * 
	 * @return the types of the record values
	 */
	public Class<?>[] getRecordTypes() {
		return recordIndex.getMeta().getTypes();
	}

	/**
	 * Gets the names of record values.
	 * 
	 * @return the names of the record values
	 */
	public String[] getRecordNames() {
		return recordIndex.getMeta().getNames();
	}

	/**
	 * Gets the {@code DataRecordIndex} created.
	 * 
	 * @return the {@code DataRecordIndex}
	 * 
	 * @see DataRecordIndex
	 */
	protected DataRecordIndex getDataRecordIndex() {
		return recordIndex;
	}

	/**
	 * Gets the {@code IntervalIndex} created.
	 * 
	 * @return the {@code IntervalIndex}
	 * 
	 * @see IntervalIndex
	 */
	protected IntervalIndex getIntervalIndex() {
		return intervalIndex;
	}

	/**
	 * Gets the {@code MetaIndex} created.
	 * 
	 * @return the {@code MetaIndex}
	 * 
	 * @see MetaIndex
	 */
	protected MetaIndex getMetaIndex() {
		return metaIndex;
	}

	/**
	 * Indexes the passed {@code record}.
	 * 
	 * @param record
	 *            the record to be indexed
	 */
	public void index(final IDataRecord record) {
		index(model.getDataStructure(), record);
	}

	/**
	 * Indexes the passed {@code record}.
	 * 
	 * @param dataStructure
	 *            the {@code DataStructure} to be used for the data
	 * @param record
	 *            the record to be indexed
	 */
	public void index(final DataStructure dataStructure,
			final IDataRecord record) {
		final int recordId = dataId;

		// make sure values still fit
		if (recordId > lastValidId || recordId < 0) {
			throw new ForwardedRuntimeException(TidaIndexException.class, 1001,
					dataId, lastValidId);
		} else {

			// increase the identifier it should never be used again
			dataId++;
		}

		// let's pre-process the record and map all the values
		final ProcessedDataRecord processedRecord = new ProcessedDataRecord(
				dataStructure, record, model, recordId);

		// now index the record
		for (final IDataRecordIndex idx : indexes.values()) {
			idx.index(processedRecord);
		}
	}

	/**
	 * Method used to optimize the index considering mainly storage.
	 */
	public void optimize() {
		for (final IDataRecordIndex idx : indexes.values()) {
			idx.optimize();
		}
	}

	/**
	 * Gets a specific instance of a used {@code IDataRecordIndex} within
	 * {@code this}.
	 * 
	 * @param clazz
	 *            the class of the {@code IDataRecordIndex} to be retrieved
	 * 
	 * @return the {@code IDataRecordIndex} used within {@code this} instance
	 */
	@SuppressWarnings("unchecked")
	protected <T extends IDataRecordIndex> T getIndex(final Class<T> clazz) {
		return (T) indexes.get(clazz);
	}

	@Override
	public void save(final BasePersistor persistor)
			throws ForwardedRuntimeException {

		// generate an identifier
		final String id = "general" + EXTENSION;
		final Identifier identifier = new Identifier(id, persistentGroup);
		identifier.setComment("General settings of the '"
				+ getClass().getSimpleName() + "'");

		final OutputStream out = persistor.openForWrite(identifier);
		try {
			persistor.writeInt(out, dataId);
		} catch (final IOException e) {
			throw new ForwardedRuntimeException(PersistorException.class, 1003,
					e, e.getMessage());
		} finally {
			persistor.close(identifier);
		}
	}

	@Override
	public void load(final BasePersistor persistor,
			final Identifier identifier, final InputStream inputStream)
			throws ForwardedRuntimeException {

		if (dataId > 0 && LOG.isWarnEnabled()) {
			LOG.debug("Loading an already filled TidaIndex (dataId == "
					+ dataId + ").");
		}

		try {
			dataId = persistor.readInt(inputStream);
		} catch (final Exception e) {
			throw new ForwardedRuntimeException(TidaIndexException.class, 1000,
					e);
		}
	}

	@Override
	public void isRegistered(final BasePersistor persistor, final Group group) {
		this.persistentGroup = group;

		for (final IDataRecordIndex idx : indexes.values()) {
			final String name = idx.getClass().getSimpleName().toLowerCase();
			final Group indexGroup = group.append(name);

			persistor.register(indexGroup, idx);
		}
	}

	@Override
	public Group getPersistentGroup() {
		return persistentGroup;
	}

	/**
	 * Gets the {@code Slice} of the specified {@code modelId} and the specified
	 * descriptor's {@code id}.
	 * 
	 * @param modelId
	 *            the identifier of the model to get the slice from
	 * @param id
	 *            the identifier of the descriptor to get the slice for
	 * 
	 * @return the {@code Slice} for the specified values, or {@code null} if
	 *         the combination of model and descriptor's identifier refers to an
	 *         unknown slice
	 */
	@SuppressWarnings({ "rawtypes", "unchecked" })
	public Slice getMetaIndexDimensionSlice(final String modelId,
			final Object id) {
		final MetaIndexDimension metaIdxDim = metaIndex.get(modelId);

		if (metaIdxDim == null) {
			return null;
		} else {
			return metaIdxDim.getSliceById(id);
		}
	}

	/**
	 * Gets the slices of the timeline for the specified range.
	 * 
	 * @param start
	 *            the start of the range
	 * @param end
	 *            the end of the range
	 * @param startInclusive
	 *            specifies if the start is included (i.e. {@code true}) or
	 *            excluded (i.e. {@code false})
	 * @param endInclusive
	 *            specifies if the end is included (i.e. {@code true}) or
	 *            excluded (i.e. {@code false})
	 * 
	 * @return the slices, which might contain {@code null} and are totally
	 *         ordered by the timeline
	 */
	public SliceWithDescriptors<?>[] getIntervalIndexSlices(final Object start,
			final Object end, final boolean startInclusive,
			final boolean endInclusive) {
		return intervalIndex.getSlicesByTimePoints(start, end, startInclusive,
				endInclusive);
	}

	/**
	 * Get the slices for the specified mapped {@code start} (included) to
	 * mapped {@code end} (included).
	 * 
	 * @param start
	 *            the mapped start point (included)
	 * @param end
	 *            the mapped end point (included)
	 * 
	 * @return the slices, which might contain {@code null} values if no data is
	 *         added to the slice yet
	 */
	public SliceWithDescriptors<?>[] getIntervalIndexSlices(final long start,
			final long end) {
		return intervalIndex.getSlices(start, end);
	}

	/**
	 * Get the slices of the {@code IntervalIndex}.
	 * 
	 * @return the slices of the {@code IntervalIndex}
	 */
	public SliceWithDescriptors<?>[] getIntervalIndexSlices() {
		return intervalIndex.getSlices();
	}

	/**
	 * Get the id of the next record which will be added.
	 * 
	 * @return the id of the next record added
	 */
	public int getNextDataId() {
		return dataId;
	}

	/**
	 * Gets the normalized start value of the index.
	 * 
	 * @return the normalized start value of the index
	 */
	public long getNormalizedTimeStart() {
		return intervalIndex.getNormStart();
	}

	/**
	 * Gets the normalized end value of the index.
	 * 
	 * @return the normalized end value of the index
	 */
	public long getNormalizedTimeEnd() {
		return intervalIndex.getNormEnd();
	}

	/**
	 * Gets the value associated to the normalizedValue.
	 * 
	 * @param normalizedValue
	 *            the normalized value
	 * 
	 * 
	 * @return the associated value
	 */
	public Object getTimePointValue(final long normalizedValue) {
		return intervalIndex.getValue(normalizedValue);
	}

	/**
	 * Gets a label, i.e. a string, for the specified {@code value}.
	 * 
	 * @param value
	 *            the value to generate a label for
	 * 
	 * @return the label for the specified value
	 */
	public String getTimePointLabel(final Object value) {
		return intervalIndex.getFormattedValue(value);
	}

	/**
	 * Gets the last record identifier.
	 * 
	 * @return the last used record identifier
	 */
	public int getLastRecordId() {
		return dataId - 1;
	}

	/**
	 * Gets the {@code Descriptor} for the specified {@code FactDescriptor}.
	 * 
	 * @param desc
	 *            the {@code FactDescriptor} to get the {@code Descriptor} for
	 * 
	 * @return the {@code Descriptor} for the specified {@code FactDescriptor}
	 *         or {@code null} if non exists
	 * 
	 * @see Descriptor
	 * @see FactDescriptor
	 */
	public Descriptor<?, ?, ?> getDescriptor(final FactDescriptor<?> desc) {
		if (desc == null) {
			return null;
		} else {
			return getDescriptor(desc.getModelId(), desc.getId());
		}
	}

	/**
	 * Gets the {@code Descriptor} for the specified {@code descModelId} and
	 * {@code descId}.
	 * 
	 * @param descModelId
	 *            the identifier of the {@code DescriptorModel} the
	 *            {@code Descriptor} belongs to
	 * @param descId
	 *            the identifier of the {@code Descriptor} to be retrieved
	 * 
	 * @return the {@code Descriptor} for the specified {@code descModelId} and
	 *         {@code descId} or {@code null} if non exists
	 * 
	 * @see Descriptor
	 * @see FactDescriptor
	 */
	public Descriptor<?, ?, ?> getDescriptor(final String descModelId,
			final Object descId) {
		return model.getMetaDataModel().getDescriptor(descModelId, descId);
	}

	/**
	 * Gets the {@code Descriptor} of the {@code Record} for the specified
	 * {@code DescriptorModel}.
	 * 
	 * @param descModelId
	 *            the identifier of the {@code DescriptorModel}
	 * @param recordId
	 *            the identifier of the record to get the {@code Descriptor} for
	 * 
	 * @return the {@code Descriptor} for the specified {@code descModelId} and
	 *         {@code recordId}, or {@code null} if non could be found
	 */
	public Descriptor<?, ?, ?> getDescriptorOfRecord(final String descModelId,
			final int recordId) {

		// make sure we have a valid record identifier
		if (recordId < 0 || recordId > getLastRecordId()) {
			return null;
		}

		// get the dimension of the DescriptorModel
		final MetaIndexDimension<?> dim = getMetaIndex().get(descModelId);
		if (dim == null) {
			return null;
		}

		// create a bitmap of the one record
		final Bitmap recBmp = Bitmap.createBitmap(getIndexFactory(), recordId);

		// check if the record is valid
		if (!recBmp.and(model.getValidRecords()).isBitSet()) {
			return null;
		}

		// search for the record's slice
		for (final Slice<?> slice : dim.getSlices()) {
			final Bitmap bmp = slice.getBitmap();
			if (recBmp.and(bmp).isBitSet()) {
				return getDescriptor(descModelId, slice.getId());
			}
		}

		return null;
	}

	/**
	 * Gets the start- and end-value of the record (or better the mapped value).
	 * 
	 * @param recordId
	 *            the identifier of the record to get the start- and end-value
	 *            for
	 * 
	 * @return {@code null} if the record is invalid or could not be found,
	 *         otherwise an array with two values, the first is the start- and
	 *         the second the end-value
	 */
	public Object[] getTimePointValuesOfRecord(final int recordId) {

		// make sure we have a valid record identifier
		if (recordId < 0 || recordId > getLastRecordId()) {
			return null;
		}

		// create a bitmap of the one record
		final Bitmap recBmp = Bitmap.createBitmap(getIndexFactory(), recordId);

		// check if the record is valid
		if (!recBmp.and(model.getValidRecords()).isBitSet()) {
			return null;
		}

		// check each value of the time-axis
		boolean searchingStart = true;
		final Object[] result = new Object[] { null, null };

		final long start = intervalIndex.getNormStart();
		final long end = intervalIndex.getNormEnd();
		for (long i = start; i <= end; i++) {
			final SliceWithDescriptors<?> slice = intervalIndex.getSliceById(i);

			if (slice == null) {
				if (searchingStart) {
					continue;
				} else {
					result[1] = intervalIndex.getValue(i - 1);
					return result;
				}
			} else {
				final Bitmap bmp = slice.getBitmap();
				if (searchingStart && recBmp.and(bmp).isBitSet()) {
					result[0] = intervalIndex.getValue(i);
					searchingStart = false;
				} else if (!searchingStart && !recBmp.and(bmp).isBitSet()) {
					result[1] = intervalIndex.getValue(i - 1);
					return result;
				}
			}
		}

		/*
		 * Check if we found a start, but no end. If no end was found, the end
		 * is the end of the time-line.
		 */
		if (!searchingStart) {
			result[1] = intervalIndex.getValue(end);
			return result;
		} else {
			return null;
		}
	}
}
