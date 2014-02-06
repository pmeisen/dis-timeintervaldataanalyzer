package net.meisen.dissertation.model.indexes.datarecord;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import net.meisen.dissertation.model.data.TidaModel;
import net.meisen.dissertation.model.datasets.IDataRecord;
import net.meisen.dissertation.model.descriptors.Descriptor;
import net.meisen.dissertation.model.descriptors.DescriptorModel;

public class TidaIndex {
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
		setMetaDataHandling((MetaDataHandling) null);
	}

	public void index(final IDataRecord record) {
		for (final DataRecordIndex idx : indexes.values()) {
			idx.index(dataId, record);
			dataId++;
		}
	}

	public MetaDataHandling getMetaDataHandling() {
		return getIndex(MetaIndex.class).getMetaDataHandling();
	}

	public void setMetaDataHandling(final MetaDataHandling metaDataHandling) {
		getIndex(MetaIndex.class).setMetaDataHandling(metaDataHandling);
	}

	@SuppressWarnings("unchecked")
	protected <T extends DataRecordIndex> T getIndex(final Class<T> clazz) {
		return (T) indexes.get(MetaIndex.class);
	}

	@SuppressWarnings({ "unchecked", "rawtypes" })
	public String getStatistic() {
		final String nl = System.getProperty("line.separator");

		final MetaIndex metaIdx = getIndex(MetaIndex.class);

		// @formatter:off
		String stat = "";
		stat += "TidaIndex statistic: " + nl;
		stat += "- MetaIndex with " + metaIdx.getAmountOfDimensions() + " dimensions:" + nl;
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
			
			// output the data use a threshold of 50 max
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
		// @formatter:on

		return stat;
	}
}
