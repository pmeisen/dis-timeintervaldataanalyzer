package net.meisen.dissertation.model.indexes.datarecord;

import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import net.meisen.dissertation.exceptions.PersistorException;
import net.meisen.dissertation.exceptions.TidaIndexException;
import net.meisen.dissertation.model.IPersistable;
import net.meisen.dissertation.model.data.TidaModel;
import net.meisen.dissertation.model.datasets.IDataRecord;
import net.meisen.dissertation.model.descriptors.Descriptor;
import net.meisen.dissertation.model.descriptors.DescriptorModel;
import net.meisen.dissertation.model.indexes.datarecord.slices.IndexDimensionSlice;
import net.meisen.dissertation.model.persistence.BasePersistor;
import net.meisen.dissertation.model.persistence.Group;
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
	private final static int STATISTIC_THRESHOLD = 30;

	private final static Logger LOG = LoggerFactory.getLogger(TidaIndex.class);

	private final Map<Class<? extends IDataRecordIndex>, IDataRecordIndex> indexes;

	private int dataId;
	private Group persistentGroup = null;

	/**
	 * Creates an index for the passed {@code model}.
	 * 
	 * @param model
	 *            the {@code TidaModel} to be added
	 * 
	 * @see TidaModel
	 */
	public TidaIndex(final TidaModel model) {
		this.dataId = 0;
		this.indexes = new HashMap<Class<? extends IDataRecordIndex>, IDataRecordIndex>();

		// add the dimensions of the MetaDataModel, Key and Interval
		indexes.put(MetaIndex.class, new MetaIndex(model));
		indexes.put(KeyIndex.class, new KeyIndex(model));
		indexes.put(BaseIntervalIndex.class, model.getIntervalModel()
				.createIndex(model.getDataStructure()));

		// set the default values
		setMetaDataHandling(model.getMetaDataHandling());
		setIntervalDataHandling(model.getIntervalDataHandling());
	}

	/**
	 * Indexes the passed {@code record}.
	 * 
	 * @param record
	 *            the record to be indexed
	 */
	public void index(final IDataRecord record) {
		for (final IDataRecordIndex idx : indexes.values()) {
			idx.index(dataId, record);
		}

		//@formatter:off
		/*
		 * dataId cannot be:
		 *  - negative or 
		 *  - greater than Integer.MAX_VALUE - EWAHCompressedBitmap.wordinbits
		 */
		//@formatter:on
		dataId++;
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
	 * Gets the {@code MetaDataHandling} defined for the index.
	 * 
	 * @return the {@code MetaDataHandling} defined for the index
	 */
	public MetaDataHandling getMetaDataHandling() {
		return getIndex(MetaIndex.class).getMetaDataHandling();
	}

	/**
	 * Sets the {@code MetaDataHandling} for the index.
	 * 
	 * @param handling
	 *            the {@code MetaDataHandling} to be used
	 */
	public void setMetaDataHandling(final MetaDataHandling handling) {
		getIndex(MetaIndex.class).setMetaDataHandling(handling);
	}

	/**
	 * Gets the {@code IntervalDataHandling} defined for the index.
	 * 
	 * @return the {@code IntervalDataHandling} defined for the index
	 */
	public IntervalDataHandling getIntervalDataHandling() {
		return getIndex(BaseIntervalIndex.class).getIntervalDataHandling();
	}

	/**
	 * Sets the {@code IntervalDataHandling} for the index.
	 * 
	 * @param handling
	 *            the {@code IntervalDataHandling} to be used
	 */
	public void setIntervalDataHandling(final IntervalDataHandling handling) {
		getIndex(BaseIntervalIndex.class).setIntervalDataHandling(handling);
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

	/**
	 * Creates a string which prints a statistics for the {@code TidaIndex}.
	 * 
	 * @return a string which can be printed to show a statistic
	 */
	@SuppressWarnings({ "unchecked", "rawtypes" })
	public String toStatistic() {
		final String nl = System.getProperty("line.separator");

		final MetaIndex metaIdx = getIndex(MetaIndex.class);
		final BaseIntervalIndex intervalIdx = getIndex(BaseIntervalIndex.class);

		String stat = "";

		// @formatter:off
		stat += "TidaIndex statistic: " + nl;
		stat += "- MetaIndex with " + metaIdx.getAmountOfDimensions() + " dimension(s):" + nl;
		for (final MetaIndexDimension<?> dim : metaIdx.getDimensions()) {
			final int amountOfSlices = dim.getAmountOfSlices();
			stat += "  - " + dim.getModelId() + " (" + amountOfSlices + " slices)" + nl;
			if (amountOfSlices == 0) {
				continue;
			}
			
			// sort the data ascending
			final List<IndexDimensionSlice> sortedSlices = new ArrayList<IndexDimensionSlice>();
			sortedSlices.addAll(Arrays.asList(dim.getSlices()));
			Collections.sort(sortedSlices, Collections.reverseOrder());
			final int nrSize = String.valueOf(Math.max(amountOfSlices, sortedSlices.get(0).count())).length();
			
			// output the data use the threshold defined by STATISTIC_THRESHOLD
			int i = 0;
			for (final IndexDimensionSlice<?> slice : sortedSlices) {
				final Object sliceId = slice.getId();
				final DescriptorModel model = dim.getModel();
				final Descriptor value = model.getDescriptor(sliceId);
				
				stat += "      + " + String.format("%1$-30s", value) + " (" + String.format("%" + nrSize + "d", slice.count()) + " records)" + nl;
				if (++i > STATISTIC_THRESHOLD) {
					stat += "      + " + String.format("%1$-30s", "...") + " [" + String.format("%" + nrSize + "d", amountOfSlices - i + 1) + " more slices]" + nl;
					break;
				}
			}
		}
		
		final int amountOfSlices = intervalIdx.getAmountOfSlices();
		stat += "- IntervalIndex for " + intervalIdx.getFormattedStart() + " (" + amountOfSlices + " slices)" + nl;
		if (amountOfSlices != 0) {
		
			// sort the data ascending
			final List<IndexDimensionSlice> sortedSlices = new ArrayList<IndexDimensionSlice>();
			sortedSlices.addAll(Arrays.asList(intervalIdx.getSlices()));
			Collections.sort(sortedSlices, new Comparator<IndexDimensionSlice>() {
				
				@Override
				public int compare(IndexDimensionSlice o1,
						IndexDimensionSlice o2) {
					
					if (o2 == null && o1 == null) {
						return 0;
					} else if (o2 == null || o1 == null) {
						return o2==null ? -1 : 1;
					} else {
						return o2.compareTo(o1);
		            }
		        }
			});
			if (sortedSlices.get(0) != null) {
				final int nrSize = String.valueOf(Math.max(amountOfSlices, sortedSlices.get(0).count())).length();
				
				// output the data use the threshold defined by STATISTIC_THRESHOLD
				int i = 0;
				for (final IndexDimensionSlice<?> slice : sortedSlices) {
					if (slice == null) {
						break;
					}
					
					final Object sliceId = slice.getId();
					final String value = intervalIdx.getFormattedId(sliceId);
					
					stat += "      + " + String.format("%1$-30s", value) + " (" + String.format("%" + nrSize + "d", slice.count()) + " records)" + nl;
					if (++i > STATISTIC_THRESHOLD) {
						stat += "      + " + String.format("%1$-30s", "...") + " [" + String.format("%" + nrSize + "d", amountOfSlices - i + 1) + " more slices]" + nl;
						break;
					}
				}
			}
		}
		// @formatter:on

		return stat;
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
		}
		persistor.close(identifier);
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
	 * Gets the {@code IndexDimensionSlice} of the specified {@code modelId} and
	 * the specified descriptor's {@code id}.
	 * 
	 * @param modelId
	 *            the identifier of the model to get the slice from
	 * @param id
	 *            the identifier of the descriptor to get the slice for
	 * 
	 * @return the {@code IndexDimensionSlice} for the specified values, or
	 *         {@code null} if the combination of model and descriptor's
	 *         identifier refers to an unknown slice
	 */
	@SuppressWarnings({ "rawtypes", "unchecked" })
	public IndexDimensionSlice getMetaIndexDimensionSlice(final String modelId,
			final Object id) {
		final MetaIndexDimension metaIdxDim = getIndex(MetaIndex.class).get(
				modelId);

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
	public IndexDimensionSlice<?>[] getIntervalIndexDimensionSlices(
			final Object start, final Object end, final boolean startInclusive,
			final boolean endInclusive) {
		final BaseIntervalIndex idx = getIndex(BaseIntervalIndex.class);
		return idx.getIntervalIndexDimensionSlices(start, end, startInclusive,
				endInclusive);
	}

	/**
	 * Get the id of the next record which will be added.
	 * 
	 * @return the id of the next record added
	 */
	public int getNextDataId() {
		return dataId;
	}
}
