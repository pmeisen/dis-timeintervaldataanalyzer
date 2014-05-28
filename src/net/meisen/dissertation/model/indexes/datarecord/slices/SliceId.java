package net.meisen.dissertation.model.indexes.datarecord.slices;

import net.meisen.dissertation.model.indexes.datarecord.IDataRecordIndex;

/**
 * An identifier used for a slice.
 * 
 * @author pmeisen
 * 
 * @param <I>
 *            the type of the id of the identifier
 */
public class SliceId<I> extends BitmapId<I> {

	/**
	 * Constructor to create a {@code SliceId} without any classifier.
	 * 
	 * @param id
	 *            the identifier
	 * @param type
	 *            the type
	 */
	public SliceId(final I id, final Class<? extends IDataRecordIndex> type) {
		super(id, type, null);
	}

	/**
	 * The constructor used to define a {@code SliceId} with {@code classifier}
	 * .
	 * 
	 * @param id
	 *            the identifier
	 * @param type
	 *            the type
	 * @param classifier
	 *            the classifier
	 */
	public SliceId(final I id, final Class<? extends IDataRecordIndex> type,
			final String classifier) {
		super(id, type, classifier);
	}
}
