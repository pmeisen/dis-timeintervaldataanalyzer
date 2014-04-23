package net.meisen.dissertation.model.indexes.datarecord.slices;

import net.meisen.dissertation.model.indexes.BaseIndexFactory;

/**
 * A slice of an index's dimension, i.e. from a data point of view the slice
 * represents a value for each record of a specific value of a dimension. Let's
 * assume we have the dimension family with different family members:
 * {@code Mom - Debbie}, {@code Dad - Philipp}, {@code Child - Edison}. A slice
 * represents one value of the dimension e.g. {@code Mom - Debbie}. The slice
 * has a value, i.e. {@code 1} or {@code 0} for each record added. Whenever a
 * record has the value {@code Mom - Debbie} the slice will have a value of
 * {@code 1} for the record's position, otherwise the value will be {@code 0}.
 * 
 * @author pmeisen
 * 
 * @param <I>
 *            the type of the identifier used to identify the slice
 */
public class Slice<I extends Object> extends BaseSlice<I> {

	/**
	 * Creates a slice with no the specified {@code recId} added (i.e.
	 * everything is set to {@code 0}) for the specified {@code sliceId}.
	 * 
	 * @param sliceId
	 *            the identifier the slice stands for
	 * @param factory
	 *            factory used to create bitmap indexes
	 * @param recordIds
	 *            the identifiers of the records to be set
	 */
	public Slice(final I sliceId, final BaseIndexFactory factory,
			final int... recordIds) {
		super(sliceId, factory, recordIds);
	}

	/**
	 * Marks the specified {@code recordIds} to be set, i.e. the value
	 * {@code this} slice represents by id is assumed to be set for the
	 * specified records.
	 * 
	 * @param recordIds
	 *            the identifiers of the records to be set
	 */
	public void set(final int... recordIds) {
		if (recordIds == null || recordIds.length == 0) {
			return;
		}

		bitmap.set(recordIds);
	}

	/**
	 * Marks the specified {@code recId} to be set, i.e. the value {@code this}
	 * slice represents by id is assumed to be set for the specified record.
	 * 
	 * @param recId
	 *            the identifiers of the record to be set
	 */
	public void set(final int recId) {
		bitmap.set(recId);
	}
}
