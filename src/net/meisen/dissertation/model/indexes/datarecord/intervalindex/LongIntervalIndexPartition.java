package net.meisen.dissertation.model.indexes.datarecord.intervalindex;

import net.meisen.dissertation.model.datastructure.IntervalStructureEntry;
import net.meisen.dissertation.model.indexes.BaseIndexedCollectionFactory;
import net.meisen.dissertation.model.indexes.IIndexedCollection;
import net.meisen.dissertation.model.indexes.datarecord.IntervalIndexPartition;
import net.meisen.dissertation.model.indexes.datarecord.slices.CombinedIndexDimensionSlice;
import net.meisen.dissertation.model.indexes.datarecord.slices.IIndexDimensionSlice;
import net.meisen.dissertation.model.indexes.datarecord.slices.IndexDimensionSlice;
import net.meisen.dissertation.model.time.mapper.BaseMapper;

/**
 * An {@code IntervalIndexPartition} is normally defined as a partition (or the
 * whole) timeline. The size of the partition is defined by a {@code Mapper},
 * which is used to map the data-values to the underlying part of the timeline.
 * 
 * @author pmeisen
 * 
 */
public class LongIntervalIndexPartition extends IntervalIndexPartition {

	/**
	 * Constructor to create a partition using the specified {@code Mapper}, the
	 * {@code start}- and {@code end}-entry and the specified
	 * {@code indexedCollectionFactory} to create the needed indexes.
	 * 
	 * @param mapper
	 *            the {@code Mapper} which defines the start and end value, as
	 *            well as the type of the indexed values
	 * @param start
	 * @param end
	 * @param indexedCollectionFactory
	 *            the {@code indexedCollectionFactory} to create the needed
	 *            indexes
	 */
	public LongIntervalIndexPartition(final BaseMapper<?> mapper,
			final IntervalStructureEntry start,
			final IntervalStructureEntry end,
			final BaseIndexedCollectionFactory indexedCollectionFactory) {
		super(mapper, start, end, indexedCollectionFactory);
	}

	@Override
	public void index(final int dataId, final Object start, final Object end) {
		final long normStart = get(start, true);
		final long normEnd = get(end, false);
		setInterval(normStart, normEnd, dataId);
	}

	/**
	 * Gets the start of the timeline (included) as {@code long}.
	 * 
	 * @return the start of the timeline as {@code long}
	 */
	public long getStart() {
		return getMapper().getNormStartAsLong();
	}

	/**
	 * Gets the end of the timeline (included) as {@code long}.
	 * 
	 * @return the end of the timeline as {@code long}
	 */
	public long getEnd() {
		return getMapper().getNormEndAsLong();
	}

	/**
	 * Get the value as {@code long}.
	 * 
	 * @param value
	 *            the value to be mapped to the {@code long}
	 * @param start
	 *            {@code true} if the value is the start value of the interval,
	 *            otherwise {@code end}
	 * 
	 * @return the mapped value as {@code long}
	 */
	protected long get(final Object value, final boolean start) {
		if (value == null) {
			return start ? getStart() : getEnd();
		} else {
			return getMapper().mapToLong(value);
		}
	}

	/**
	 * And-combines the slices for the specified {@code start} (included) to
	 * {@code end} (included).
	 * 
	 * @param start
	 *            the start point (included)
	 * @param end
	 *            the end point (included)
	 * 
	 * @return the result of the combination of the specified slices (by and)
	 */
	public CombinedIndexDimensionSlice and(final long start, final long end) {

		// combine the slices
		final CombinedIndexDimensionSlice combinedSlice = new CombinedIndexDimensionSlice();
		combinedSlice.and(getIndex().getObjectsByStartAndEnd(start, end));

		// return the result
		return combinedSlice;
	}

	/**
	 * Or-combines the slices for the specified {@code start} (included) to
	 * {@code end} (included).
	 * 
	 * @param start
	 *            the start point (included)
	 * @param end
	 *            the end point (included)
	 * 
	 * @return the result of the combination of the specified slices (by or)
	 */
	public CombinedIndexDimensionSlice or(final long start, final long end) {

		// combine the slices
		final CombinedIndexDimensionSlice combinedSlice = new CombinedIndexDimensionSlice();
		combinedSlice.or(getIndex().getObjectsByStartAndEnd(start, end));

		// return the result
		return combinedSlice;
	}

	/**
	 * Get the slices for the specified {@code start} (included) to {@code end}
	 * (included).
	 * 
	 * @param start
	 *            the start point (included)
	 * @param end
	 *            the end point (included)
	 * 
	 * @return the slices, which might be {@code null} if no data is there yet
	 */
	public IIndexDimensionSlice[] getSlices(final long start, final long end) {
		return castSlices(getIndex().getObjectsByStartAndEnd(start, end));
	}

	/**
	 * Gets a slice of the partition, i.e. a bitmap which defines which records
	 * have the value of the specified slice set (i.e. {@code 1}) and which
	 * don't (i.e. {@code 0}).
	 * 
	 * @param point
	 *            the identifier of the value, i.e. the identifier of the value
	 *            of the partition to retrieve the information for
	 * @return a bitmap with the identifiers of the records set to {@code 1} if
	 *         and only if the record's value is referred by the specified
	 *         {@code point}
	 */
	@SuppressWarnings("unchecked")
	public IndexDimensionSlice<Long> getSliceById(final long point) {
		return (IndexDimensionSlice<Long>) getIndex().getObject(point);
	}

	/**
	 * Sets the specified interval [{@code normStart}, {@code normEnd}] to true.
	 * 
	 * @param normStart
	 *            the start of the interval (included)
	 * @param normEnd
	 *            the end of the interval (included)
	 * @param recId
	 *            the value to be set to true within the slice
	 */
	protected void setInterval(final long normStart, final long normEnd,
			final int recId) {
		final IIndexedCollection index = getIndex();

		for (long i = normStart; i < normEnd + 1; i++) {
			final IndexDimensionSlice<Long> slice = getSliceById(i);
			if (slice == null) {
				index.addObject(new IndexDimensionSlice<Long>(i, recId));
			} else {
				slice.set(recId);
			}
		}
	}
}
