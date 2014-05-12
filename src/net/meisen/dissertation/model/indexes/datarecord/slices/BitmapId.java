package net.meisen.dissertation.model.indexes.datarecord.slices;

import java.util.Arrays;

import net.meisen.dissertation.model.indexes.datarecord.IDataRecordIndex;
import net.meisen.general.genmisc.types.Streams;
import net.meisen.general.genmisc.types.Strings;

public class BitmapId<I> {

	private final I id;
	private final Class<? extends IDataRecordIndex> type;
	private final String[] classifier;

	private byte[] byteRepresentative = null;
	private Integer hashCode = null;

	public BitmapId(final I id, final Class<? extends IDataRecordIndex> type,
			final String... classifier) {
		this.id = id;
		this.type = type;
		this.classifier = classifier;
	}

	@SuppressWarnings("unchecked")
	public Class<I> getIdType() {
		return (Class<I>) getId().getClass();
	}

	public I getId() {
		return id;
	}

	public Class<? extends IDataRecordIndex> getType() {
		return type;
	}

	public String[] getClassifier() {
		if (classifier == null) {
			return new String[0];
		} else {
			return classifier;
		}
	}

	@Override
	public String toString() {
		final String c = Strings.join(".", getClassifier());

		if (c.isEmpty()) {
			return c + "." + getId() + " (" + getType().getSimpleName() + ")";
		} else {
			return getId() + " (" + getType().getSimpleName() + ")";
		}
	}

	@Override
	public int hashCode() {
		if (hashCode == null) {
			hashCode = Arrays.hashCode(new Object[] { getId(), getType(),
					getClassifier() });
		}
		
		return hashCode.intValue();
	}

	public byte[] bytes() {

		if (byteRepresentative == null) {

			// get the known objects
			final byte[] bytesId = Streams.objectToByte(getId());
			final byte[] bytesClass = Streams.objectToByte(getType());
			final byte[] bytesClassifierSize = Streams
					.intToByte(getClassifier().length);

			// get the different classifiers
			byte[] bytesClassifier = new byte[0];
			for (final String classifier : getClassifier()) {
				bytesClassifier = Streams.combineBytes(bytesClassifier,
						Streams.objectToByte(classifier));
			}

			// combine all the bytes
			byteRepresentative = Streams.combineBytes(bytesId, bytesClass,
					bytesClassifierSize, bytesClassifier);
		}

		return byteRepresentative;
	}
}
