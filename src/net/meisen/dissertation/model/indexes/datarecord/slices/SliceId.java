package net.meisen.dissertation.model.indexes.datarecord.slices;

import net.meisen.dissertation.model.indexes.datarecord.IDataRecordIndex;

public class SliceId<I> extends BitmapId<I> {

	public SliceId(final I id, final Class<? extends IDataRecordIndex> type) {
		super(id, type, null);
	}

	public SliceId(final I id, final Class<? extends IDataRecordIndex> type,
			final String classifier) {
		super(id, type, classifier);
	}
}
