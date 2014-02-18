package net.meisen.dissertation.model.indexes.datarecord;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import net.meisen.dissertation.model.data.DataModel;
import net.meisen.dissertation.model.data.TidaModel;
import net.meisen.dissertation.model.datasets.IClosableIterator;
import net.meisen.dissertation.model.datasets.IDataRecord;
import net.meisen.dissertation.model.descriptors.Descriptor;
import net.meisen.dissertation.model.descriptors.DescriptorModel;
import net.meisen.dissertation.model.indexes.datarecord.slices.IndexDimensionSlice;

/**
 * An index for {@code TimeIntervalDataAnalysis}. The index is used to select
 * {@code DataRecords} efficient considering the querying time.
 * 
 * @author pmeisen
 * 
 */
public class TidaIndex {
	private final static Logger LOG = LoggerFactory.getLogger(TidaIndex.class);
	private final static int STATISTIC_THRESHOLD = 30;

	private final Map<Class<? extends DataRecordIndex>, DataRecordIndex> indexes;

	private int dataId;

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
		
		for (final DataRecordIndex idx : indexes.values()) {
			idx.optimize();
		}
	}

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
			i++;

			if (LOG.isDebugEnabled() && (i % 10000 == 0)) {
				LOG.debug("... added " + i + " records from dataModel...");
			}
		}
		it.close();

		// log the finalization
		if (LOG.isDebugEnabled()) {
			LOG.debug("Finished adding of " + i + " records from dataModel.");
		}
	}

	public MetaDataHandling getMetaDataHandling() {
		return getIndex(MetaIndex.class).getMetaDataHandling();
	}

	public void setMetaDataHandling(final MetaDataHandling handling) {
		getIndex(MetaIndex.class).setMetaDataHandling(handling);
	}

	public IntervalDataHandling getIntervalDataHandling() {
		return getIndex(IntervalIndex.class).getIntervalDataHandling();
	}

	public void setIntervalDataHandling(final IntervalDataHandling handling) {
		getIndex(IntervalIndex.class).setIntervalDataHandling(handling);
	}

	@SuppressWarnings("unchecked")
	protected <T extends DataRecordIndex> T getIndex(final Class<T> clazz) {
		return (T) indexes.get(clazz);
	}

	@SuppressWarnings({ "unchecked", "rawtypes" })
	public String getStatistic() {
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
				break;
			}
			
			// sort the data ascending
			final List<IndexDimensionSlice> sortedSlices = new ArrayList<IndexDimensionSlice>();
			sortedSlices.addAll(dim.getSlices());
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
				break;
			}
			
			// sort the data ascending
			final List<IndexDimensionSlice> sortedSlices = new ArrayList<IndexDimensionSlice>();
			sortedSlices.addAll(part.getSlices());
			Collections.sort(sortedSlices, Collections.reverseOrder());
			final int nrSize = String.valueOf(Math.max(amountOfSlices, sortedSlices.get(0).get().length)).length();
			
			// output the data use the threshold defined by STATISTIC_THRESHOLD
			int i = 0;
			for (final IndexDimensionSlice<?> slice : sortedSlices) {
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
}
