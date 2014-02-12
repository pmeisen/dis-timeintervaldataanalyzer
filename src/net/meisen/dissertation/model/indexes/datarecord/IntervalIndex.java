package net.meisen.dissertation.model.indexes.datarecord;

import java.util.Collection;

import net.meisen.dissertation.model.data.DataStructure;
import net.meisen.dissertation.model.data.IntervalModel;
import net.meisen.dissertation.model.data.TidaModel;
import net.meisen.dissertation.model.datasets.IDataRecord;
import net.meisen.dissertation.model.indexes.IIndexedCollection;

public class IntervalIndex implements DataRecordIndex {

	private final IIndexedCollection timeIndex;

	private IntervalDataHandling intervalDataHandling;

	public IntervalIndex(final TidaModel model) {
		this(model.getIntervalModel(), model.getDataStructure());
	}

	public IntervalIndex(final IntervalModel intervalModel,
			final DataStructure dataStructure) {
		this.timeIndex = intervalModel.createIndex(dataStructure);

		setIntervalDataHandling(null);
	}

	@Override
	public void index(final int dataId, final IDataRecord record) {
		// TODO Auto-generated method stub
	}

	public IntervalDataHandling getIntervalDataHandling() {
		return intervalDataHandling;
	}

	public void setIntervalDataHandling(final IntervalDataHandling handling) {
		this.intervalDataHandling = handling == null ? IntervalDataHandling
				.find(null) : handling;
	}

	/**
	 * Gets the dimensions of the {@code MetaIndex} as collection.
	 * 
	 * @return the dimensions of the {@ode MetaIndex}
	 */
	@SuppressWarnings("unchecked")
	protected Collection<IntervalIndexDimension> getDimensions() {
		return (Collection<IntervalIndexDimension>) timeIndex.getAll();
	}
}
