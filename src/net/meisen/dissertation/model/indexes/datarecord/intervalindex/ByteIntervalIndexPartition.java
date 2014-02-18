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
public class ByteIntervalIndexPartition extends IntervalIndexPartition {

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
	public ByteIntervalIndexPartition(final BaseMapper<?> mapper,
			final IntervalStructureEntry start,
			final IntervalStructureEntry end,
			final BaseIndexedCollectionFactory indexedCollectionFactory) {
		super(mapper, start, end, indexedCollectionFactory);
	}

	@Override
	public void index(final int dataId, final Object start, final Object end) {
		final byte normStart = get(start, true);
		final byte normEnd = get(end, false);
		setInterval(normStart, normEnd, dataId);
	}

	/**
	 * Gets the start of the timeline (included) as {@code byte}.
	 * 
	 * @return the start of the timeline as {@code byte}
	 */
	public byte getStart() {
		return getMapper().getNormStartAsByte();
	}

	/**
	 * Gets the end of the timeline (included) as {@code byte}.
	 * 
	 * @return the end of the timeline as {@code byte}
	 */
	public byte getEnd() {
		return getMapper().getNormEndAsByte();
	}

	/**
	 * Get the value as {@code byte}.
	 * 
	 * @param value
	 *            the value to be mapped to the {@code byte}
	 * @param start
	 *            {@code true} if the value is the start value of the interval,
	 *            otherwise {@code end}
	 * 
	 * @return the mapped value as {@code byte}
	 */
	protected byte get(final Object value, final boolean start) {
		if (value == null) {
			return start ? getStart() : getEnd();
		} else {
			return getMapper().mapToByte(value);
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
	public CombinedIndexDimensionSlice and(final byte start, final byte end) {

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
	public CombinedIndexDimensionSlice or(final byte start, final byte end) {

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
	public IIndexDimensionSlice[] getSlices(final byte start, final byte end) {
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
	public IndexDimensionSlice<Byte> getSliceById(final byte point) {
		return (IndexDimensionSlice<Byte>) getIndex().getObject(point);
	}

	/**
	 * Set the values of the interval within the index.
	 * 
	 * @param normStart
	 *            the
	 * @param normEnd
	 * @param recId
	 */
	protected void setInterval(final byte normStart, final byte normEnd,
			final int recId) {
		final IIndexedCollection index = getIndex();

		for (byte i = normStart; i < normEnd + 1; i++) {
			final IndexDimensionSlice<Byte> slice = getSliceById(i);
			if (slice == null) {
				index.addObject(new IndexDimensionSlice<Byte>(i, recId));
			} else {
				slice.set(recId);
			}
		}
	}
}
