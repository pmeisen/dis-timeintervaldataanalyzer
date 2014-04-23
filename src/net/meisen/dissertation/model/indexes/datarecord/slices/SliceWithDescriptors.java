package net.meisen.dissertation.model.indexes.datarecord.slices;

import net.meisen.dissertation.model.indexes.BaseIndexFactory;

public class SliceWithDescriptors<I> extends BaseSlice<I> {
	private final DescriptorsSet descriptors;

	public SliceWithDescriptors(final I sliceId,
			final BaseIndexFactory factory, int... recordIds) {
		super(sliceId, factory, recordIds);
		descriptors = new DescriptorsSet();
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
