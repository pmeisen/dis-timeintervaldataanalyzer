package net.meisen.dissertation.model.indexes.datarecord;

import java.util.Collection;

import com.google.common.base.Objects;

import net.meisen.dissertation.model.data.DataStructure;
import net.meisen.dissertation.model.data.IntervalModel;
import net.meisen.dissertation.model.data.TidaModel;
import net.meisen.dissertation.model.datasets.IDataRecord;
import net.meisen.dissertation.model.indexes.IIndexedCollection;

/**
 * An {@code IntervalIndex} is used to index intervals read from a
 * {@code DataRecord}. It is mainly focused on analyzing purposes, i.e. adding
 * and updating might be slow, but accessing, counting, aggregation is quiet
 * fast.
 * 
 * @author pmeisen
 * 
 * @see IDataRecord
 * 
 */
public class IntervalIndex implements DataRecordIndex {

	private final IIndexedCollection timeIndex;

	private IntervalDataHandling intervalDataHandling;

	/**
	 * Creates an {@code IntervalIndex} for the {@code TidaModel}. The
	 * {@code TidaModel} which defines the data to be indexed.
	 * 
	 * @param model
	 *            the {@code TidaModel} defining the data to be indexed
	 * 
	 * @see TidaModel
	 */
	public IntervalIndex(final TidaModel model) {
		this(model.getIntervalModel(), model.getDataStructure());
	}

	/**
	 * Creates an {@code IntervalIndex} for the specified {@code IntervalModel}
	 * and {@code DataStructure}.
	 * 
	 * @param intervalModel
	 *            the {@code IntervalModel}
	 * @param dataStructure
	 *            the {@code DataStructure}
	 * 
	 * @see IntervalModel
	 * @see DataStructure
	 */
	public IntervalIndex(final IntervalModel intervalModel,
			final DataStructure dataStructure) {
		this.timeIndex = intervalModel.createIndex(dataStructure);

		setIntervalDataHandling(null);
	}

	@Override
	public void index(final int dataId, final IDataRecord record) {
		for (final IntervalIndexPartition part : getPartitions()) {
			part.index(dataId, record);
		}
	}

	/**
	 * Gets the handling of missing interval data.
	 * 
	 * @return the handling of missing interval data
	 */
	public IntervalDataHandling getIntervalDataHandling() {
		return intervalDataHandling;
	}

	/**
	 * Defines the handling of missing interval data.
	 * 
	 * @param handling
	 *            the handling of missing interval data
	 */
	public void setIntervalDataHandling(final IntervalDataHandling handling) {
		final IntervalDataHandling newIntervalDataHandling;

		if (this.intervalDataHandling == null) {
			newIntervalDataHandling = handling;
		} else {
			newIntervalDataHandling = handling == null ? IntervalDataHandling
					.find(null) : handling;
			if (Objects.equal(this.intervalDataHandling,
					newIntervalDataHandling)) {
				return;
			}
		}

		// set the new value and apply it to all other
		this.intervalDataHandling = newIntervalDataHandling;
		for (final IntervalIndexPartition part : getPartitions()) {
			part.setIntervalDataHandling(this.intervalDataHandling);
		}
	}

	/**
	 * Gets the amount of partitions indexed by {@code this}.
	 * 
	 * @return the amount of partitions indexed
	 */
	public int getAmountOfPartitions() {
		return timeIndex.size();
	}

	/**
	 * Gets the partitions of the {@code IntervalIndex} as collection.
	 * 
	 * @return the partitions of the {@ode IntervalIndex}
	 */
	@SuppressWarnings("unchecked")
	protected Collection<IntervalIndexPartition> getPartitions() {
		return (Collection<IntervalIndexPartition>) timeIndex.getAll();
	}
}
