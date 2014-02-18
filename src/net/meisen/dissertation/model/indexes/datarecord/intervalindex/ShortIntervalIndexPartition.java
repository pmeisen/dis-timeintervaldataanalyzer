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
public class ShortIntervalIndexPartition extends IntervalIndexPartition {

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
	public ShortIntervalIndexPartition(final BaseMapper<?> mapper,
			final IntervalStructureEntry start,
			final IntervalStructureEntry end,
			final BaseIndexedCollectionFactory indexedCollectionFactory) {
		super(mapper, start, end, indexedCollectionFactory);
	}

	@Override
	public void index(final int dataId, final Object start, final Object end) {
		final short normStart = get(start, true);
		final short normEnd = get(end, false);
		setInterval(normStart, normEnd, dataId);
	}

	/**
	 * Gets the start of the timeline (included) as {@code short}.
	 * 
	 * @return the start of the timeline as {@code short}
	 */
	public short getStart() {
		return getMapper().getNormStartAsShort();
	}

	/**
	 * Gets the end of the timeline (included) as {@code short}.
	 * 
	 * @return the end of the timeline as {@code short}
	 */
	public short getEnd() {
		return getMapper().getNormEndAsShort();
	}

	/**
	 * Get the value as {@code short}.
	 * 
	 * @param value
	 *            the value to be mapped to the {@code short}
	 * @param start
	 *            {@code true} if the value is the start value of the interval,
	 *            otherwise {@code end}
	 * 
	 * @return the mapped value as {@code short}
	 */
	protected short get(final Object value, final boolean start) {
		if (value == null) {
			return start ? getStart() : getEnd();
		} else {
			return getMapper().mapToShort(value);
		}
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
	public IndexDimensionSlice<Short> getSliceById(final short point) {
		return (IndexDimensionSlice<Short>) getIndex().getObject(point);
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
	public CombinedIndexDimensionSlice and(final short start, final short end) {

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
	public CombinedIndexDimensionSlice or(final short start, final short end) {

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
	public IIndexDimensionSlice[] getSlices(final short start, final short end) {
		return castSlices(getIndex().getObjectsByStartAndEnd(start, end));
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
	protected void setInterval(final short normStart, final short normEnd,
			final int recId) {
		final IIndexedCollection index = getIndex();

		for (short i = normStart; i < normEnd + 1; i++) {
			final IndexDimensionSlice<Short> slice = getSliceById(i);
			if (slice == null) {
				index.addObject(new IndexDimensionSlice<Short>(i, recId));
			} else {
				slice.set(recId);
			}
		}
	}
}
