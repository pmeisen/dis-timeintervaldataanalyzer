package net.meisen.dissertation.impl.indexes.datarecord.intervalindex;

import net.meisen.dissertation.model.datastructure.IntervalStructureEntry;
import net.meisen.dissertation.model.indexes.BaseIndexFactory;
import net.meisen.dissertation.model.indexes.IIndexedCollection;
import net.meisen.dissertation.model.indexes.datarecord.BaseIntervalIndex;
import net.meisen.dissertation.model.indexes.datarecord.bitmap.Bitmap;
import net.meisen.dissertation.model.indexes.datarecord.slices.IndexDimensionSlice;
import net.meisen.dissertation.model.time.mapper.BaseMapper;
import net.meisen.general.genmisc.types.Numbers;

/**
 * An {@code IntervalIndex} is normally defined as a index (for the whole)
 * timeline. The size of the index is defined by a {@code Mapper}, which is used
 * to map the data-values to the underlying part of the timeline.
 * 
 * @author pmeisen
 * 
 */
public class LongIntervalIndex extends BaseIntervalIndex {

	/**
	 * Constructor to create a index using the specified {@code Mapper}, the
	 * {@code start}- and {@code end}-entry and the specified
	 * {@code IndexFactory} to create the needed indexes.
	 * 
	 * @param mapper
	 *            the {@code Mapper} which defines the start and end value, as
	 *            well as the type of the indexed values
	 * @param start
	 *            the start entry
	 * @param end
	 *            the end entry
	 * @param indexFactory
	 *            the {@code indexFactory} to create the needed indexes
	 */
	public LongIntervalIndex(final BaseMapper<?> mapper,
			final IntervalStructureEntry start,
			final IntervalStructureEntry end,
			final BaseIndexFactory indexFactory) {
		super(mapper, start, end, indexFactory);
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
	public Bitmap and(final long start, final long end) {
		return Bitmap.and(getIndexFactory(), getIndex()
				.getObjectsByStartAndEnd(start, end));
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
	public Bitmap or(final long start, final long end) {
		return Bitmap.or(getIndexFactory(),
				getIndex().getObjectsByStartAndEnd(start, end));
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
	public IndexDimensionSlice<?>[] getSlices(final long start, final long end) {
		return castSlices(getIndex().getObjectsByStartAndEnd(start, end));
	}

	@Override
	public IndexDimensionSlice<?>[] getSlices() {
		return getSlices(getStart(), getEnd());
	}

	/**
	 * Gets a slice of the index, i.e. a bitmap which defines which records have
	 * the value of the specified slice set (i.e. {@code 1}) and which don't
	 * (i.e. {@code 0}).
	 * 
	 * @param point
	 *            the identifier of the value, i.e. the identifier of the value
	 *            of the index to retrieve the information for
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
				index.addObject(createSlice(i, recId));
			} else {
				slice.set(recId);
			}
		}
	}

	@Override
	protected IndexDimensionSlice<Long> createSlice(final Number sliceId,
			final int... recordIds) {
		return new IndexDimensionSlice<Long>(Numbers.castToLong(sliceId),
				getIndexFactory(), recordIds);
	}
	
	@Override
	public IndexDimensionSlice<?>[] getIntervalIndexDimensionSlices(
			final Object start, final Object end, final boolean startInclusive,
			final boolean endInclusive) {
		long startLong = getMapper().mapToLong(start);
		long endLong = getMapper().mapToLong(end);
		
		if (!startInclusive) {
			startLong = Numbers.castToLong(startLong + 1);
		}
		if (!endInclusive) {
			endLong = Numbers.castToLong(endLong - 1);
		}

		return getSlices(startLong, endLong);
	}
}
