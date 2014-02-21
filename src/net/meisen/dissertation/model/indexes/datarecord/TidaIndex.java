package net.meisen.dissertation.model.indexes.datarecord;

import java.io.InputStream;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import net.meisen.dissertation.model.IPersistable;
import net.meisen.dissertation.model.data.DataModel;
import net.meisen.dissertation.model.data.TidaModel;
import net.meisen.dissertation.model.datasets.IClosableIterator;
import net.meisen.dissertation.model.datasets.IDataRecord;
import net.meisen.dissertation.model.descriptors.Descriptor;
import net.meisen.dissertation.model.descriptors.DescriptorModel;
import net.meisen.dissertation.model.indexes.datarecord.slices.IndexDimensionSlice;
import net.meisen.dissertation.model.persistence.Group;
import net.meisen.dissertation.model.persistence.BasePersistor;
import net.meisen.dissertation.model.persistence.Identifier;

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
	private final static int STATISTIC_THRESHOLD = 30;
	private final static Logger LOG = LoggerFactory.getLogger(TidaIndex.class);

	private final Map<Class<? extends DataRecordIndex>, DataRecordIndex> indexes;

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
		this.indexes = new HashMap<Class<? extends DataRecordIndex>, DataRecordIndex>();

		// add the dimensions of the MetaDataModel, Key and Interval
		indexes.put(MetaIndex.class, new MetaIndex(model));
		indexes.put(KeyIndex.class, new KeyIndex(model));
		indexes.put(IntervalIndex.class, new IntervalIndex(model));

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
		for (final DataRecordIndex idx : indexes.values()) {
			idx.index(dataId, record);

			//@formatter:off
			/*
			 * dataId cannot be:
			 *  - negative or 
			 *  - greater than Integer.MAX_VALUE - EWAHCompressedBitmap.wordinbits
			 */
			//@formatter:on
			dataId++;
		}
	}

	/**
	 * Method used to optimize the index considering mainly storage.
	 */
	public void optimize() {
		for (final DataRecordIndex idx : indexes.values()) {
			idx.optimize();
		}
	}

	/**
	 * Indexes the data available within the {@code DataModel}.
	 * 
	 * @param dataModel
	 *            the {@code DataModel} to retrieve data from
	 */
	public void index(final DataModel dataModel) {

		// log the start
		if (LOG.isDebugEnabled()) {
			LOG.debug("Start adding of records from dataModel...");
		}

		// check the data and add it to the initialize index
		final IClosableIterator<IDataRecord> it = dataModel.iterator();
		int i = 0;
		while (it.hasNext()) {
			index(it.next());

			if (++i % 10000 == 0 && LOG.isDebugEnabled()) {
				LOG.debug("... added " + i + " records from dataModel...");
			}
		}
		it.close();

		// log the finalization
		if (LOG.isDebugEnabled()) {
			LOG.debug("Finished adding of " + i + " records from dataModel.");
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
		return getIndex(IntervalIndex.class).getIntervalDataHandling();
	}

	/**
	 * Sets the {@code IntervalDataHandling} for the index.
	 * 
	 * @param handling
	 *            the {@code IntervalDataHandling} to be used
	 */
	public void setIntervalDataHandling(final IntervalDataHandling handling) {
		getIndex(IntervalIndex.class).setIntervalDataHandling(handling);
	}

	/**
	 * Gets a specific instance of a used {@code DataRecordIndex} within
	 * {@code this}.
	 * 
	 * @param clazz
	 *            the class of the {@code DataRecordIndex} to be retrieved
	 * 
	 * @return the {@code DataRecordIndex} used within {@code this} instance
	 */
	@SuppressWarnings("unchecked")
	protected <T extends DataRecordIndex> T getIndex(final Class<T> clazz) {
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
		final IntervalIndex intervalIdx = getIndex(IntervalIndex.class);

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
			final int nrSize = String.valueOf(Math.max(amountOfSlices, sortedSlices.get(0).get().length)).length();
			
			// output the data use the threshold defined by STATISTIC_THRESHOLD
			int i = 0;
			for (final IndexDimensionSlice<?> slice : sortedSlices) {
				final Object sliceId = slice.getId();
				final DescriptorModel model = dim.getModel();
				final Descriptor value = model.getDescriptor(sliceId);
				
				stat += "      + " + String.format("%1$-30s", value) + " (" + String.format("%" + nrSize + "d", slice.get().length) + " records)" + nl;
				if (++i > STATISTIC_THRESHOLD) {
					stat += "      + " + String.format("%1$-30s", "...") + " [" + String.format("%" + nrSize + "d", amountOfSlices - i + 1) + " more slices]" + nl;
					break;
				}
			}
		}
		
		stat += "- IntervalIndex with " + intervalIdx.getAmountOfPartitions() + " partition(s):" + nl;
		for (final IntervalIndexPartition part : intervalIdx.getPartitions()) {
			final int amountOfSlices = part.getAmountOfSlices();
			stat += "  - " + part.getPartitionId() + " (" + amountOfSlices + " slices)" + nl;
			if (amountOfSlices == 0) {
				continue;
			}
			
			// sort the data ascending
			final List<IndexDimensionSlice> sortedSlices = new ArrayList<IndexDimensionSlice>();
			sortedSlices.addAll(Arrays.asList(part.getSlices()));
			Collections.sort(sortedSlices, new Comparator<IndexDimensionSlice>() {
				
				@Override
				public int compare(IndexDimensionSlice o1,
						IndexDimensionSlice o2) {
					
					if (o2 == null && o1 == null) {
						return 0;
					}else if (o2 == null || o1 == null) {
						return o2==null ? -1 : 1;
					}  else {
						return o2.compareTo(o1);
		            }
		        }
			});
			if (sortedSlices.get(0) == null) {
				continue;
			}
			final int nrSize = String.valueOf(Math.max(amountOfSlices, sortedSlices.get(0).get().length)).length();
			
			// output the data use the threshold defined by STATISTIC_THRESHOLD
			int i = 0;
			for (final IndexDimensionSlice<?> slice : sortedSlices) {
				if (slice == null) {
					break;
				}
				
				final Object sliceId = slice.getId();
				final String value = part.getFormattedId(sliceId);
				
				stat += "      + " + String.format("%1$-30s", value) + " (" + String.format("%" + nrSize + "d", slice.get().length) + " records)" + nl;
				if (++i > STATISTIC_THRESHOLD) {
					stat += "      + " + String.format("%1$-30s", "...") + " [" + String.format("%" + nrSize + "d", amountOfSlices - i + 1) + " more slices]" + nl;
					break;
				}
			}
		}
		// @formatter:on

		return stat;
	}

	@Override
	public void save(final BasePersistor persistor) {
		// nothing to save, the indexes are added via registration
	}

	@Override
	public void load(final BasePersistor persistor,
			final Identifier identifier, final InputStream inputStream) {
		throw new IllegalStateException("The '" + getClass().getSimpleName()
				+ "' does not save anything which should be loaded.");
	}

	@Override
	public void isRegistered(final BasePersistor persistor, final Group group) {
		this.persistentGroup = group;

		for (final DataRecordIndex idx : indexes.values()) {
			final String name = idx.getClass().getSimpleName().toLowerCase();
			final Group indexGroup = group.append(name);

			persistor.register(indexGroup, idx);
		}
	}

	@Override
	public Group getPersistentGroup() {
		return persistentGroup;
	}
}
