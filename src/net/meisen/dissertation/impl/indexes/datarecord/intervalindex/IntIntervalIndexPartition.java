package net.meisen.dissertation.impl.indexes.datarecord.intervalindex;

import net.meisen.dissertation.model.datastructure.IntervalStructureEntry;
import net.meisen.dissertation.model.indexes.BaseIndexedCollectionFactory;
import net.meisen.dissertation.model.indexes.IIndexedCollection;
import net.meisen.dissertation.model.indexes.datarecord.BaseIntervalIndexPartition;
import net.meisen.dissertation.model.indexes.datarecord.bitmap.Bitmap;
import net.meisen.dissertation.model.indexes.datarecord.slices.IndexDimensionSlice;
import net.meisen.dissertation.model.time.mapper.BaseMapper;
import net.meisen.general.genmisc.types.Numbers;

/**
 * An {@code IntervalIndexPartition} is normally defined as a partition (or the
 * whole) timeline. The size of the partition is defined by a {@code Mapper},
 * which is used to map the data-values to the underlying part of the timeline.
 * 
 * @author pmeisen
 * 
 */
public class IntIntervalIndexPartition extends BaseIntervalIndexPartition {

	/**
	 * Constructor to create a partition using the specified {@code Mapper}, the
	 * {@code start}- and {@code end}-entry and the specified
	 * {@code indexedCollectionFactory} to create the needed indexes.
	 * 
	 * @param mapper
	 *            the {@code Mapper} which defines the start and end value, as
	 *            well as the type of the indexed values
	 * @param start
	 *            the start entry
	 * @param end
	 *            the end entry
	 * @param indexedCollectionFactory
	 *            the {@code indexedCollectionFactory} to create the needed
	 *            indexes
	 */
	public IntIntervalIndexPartition(final BaseMapper<?> mapper,
			final IntervalStructureEntry start,
			final IntervalStructureEntry end,
			final BaseIndexedCollectionFactory indexedCollectionFactory) {
		super(mapper, start, end, indexedCollectionFactory);
	}

	@Override
	public void index(final int dataId, final Object start, final Object end) {
		final int normStart = get(start, true);
		final int normEnd = get(end, false);
		setInterval(normStart, normEnd, dataId);
	}

	/**
	 * Gets the start of the timeline (included) as {@code int}.
	 * 
	 * @return the start of the timeline as {@code int}
	 */
	public int getStart() {
		return getMapper().getNormStartAsInt();
	}

	/**
	 * Gets the end of the timeline (included) as {@code int}.
	 * 
	 * @return the end of the timeline as {@code int}
	 */
	public int getEnd() {
		return getMapper().getNormEndAsInt();
	}

	/**
	 * Get the value as {@code int}.
	 * 
	 * @param value
	 *            the value to be mapped to the {@code int}
	 * @param start
	 *            {@code true} if the value is the start value of the interval,
	 *            otherwise {@code end}
	 * 
	 * @return the mapped value as {@code int}
	 */
	protected int get(final Object value, final boolean start) {
		if (value == null) {
			return start ? getStart() : getEnd();
		} else {
			return getMapper().mapToInt(value);
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
	public Bitmap and(final int start, final int end) {
		return Bitmap.and(getIndexedCollectionFactory(), getIndex()
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
	public Bitmap or(final int start, final int end) {
		return Bitmap.or(getIndexedCollectionFactory(), getIndex()
				.getObjectsByStartAndEnd(start, end));
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
	public IndexDimensionSlice<?>[] getSlices(final int start, final int end) {
		return castSlices(getIndex().getObjectsByStartAndEnd(start, end));
	}

	@Override
	public IndexDimensionSlice<?>[] getSlices() {
		return getSlices(getStart(), getEnd());
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
	public IndexDimensionSlice<Integer> getSliceById(final int point) {
		return (IndexDimensionSlice<Integer>) getIndex().getObject(point);
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
	protected void setInterval(final int normStart, final int normEnd,
			final int recId) {
		final IIndexedCollection index = getIndex();

		for (int i = normStart; i < normEnd + 1; i++) {
			final IndexDimensionSlice<Integer> slice = getSliceById(i);
			if (slice == null) {
				index.addObject(createSlice(i, recId));
			} else {
				slice.set(recId);
			}
		}
	}

	@Override
	protected IndexDimensionSlice<Integer> createSlice(final Number sliceId,
			final int... recordIds) {
		return new IndexDimensionSlice<Integer>(Numbers.castToInt(sliceId),
				getIndexedCollectionFactory(), recordIds);
	}
}
