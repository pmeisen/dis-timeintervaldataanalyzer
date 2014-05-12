package net.meisen.dissertation.model.indexes.datarecord.slices;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import net.meisen.dissertation.model.indexes.datarecord.IDataRecordIndex;
import net.meisen.dissertation.model.indexes.datarecord.IntervalIndex;
import net.meisen.dissertation.model.indexes.datarecord.MetaIndexDimension;
import net.meisen.general.genmisc.collections.Collections;
import net.meisen.general.genmisc.types.Objects;
import net.meisen.general.genmisc.types.Streams;
import net.meisen.general.genmisc.types.Strings;

public class BitmapId<I> {

	private final static List<Class<? extends IDataRecordIndex>> bitmapClasses;
	static {
		bitmapClasses = new ArrayList<Class<? extends IDataRecordIndex>>(2);

		bitmapClasses.add(MetaIndexDimension.class);
		bitmapClasses.add(IntervalIndex.class);
	}
	private final static List<Class<?>> idTypes;
	static {
		idTypes = new ArrayList<Class<?>>(5);

		idTypes.add(Object.class);
		idTypes.add(Byte.class);
		idTypes.add(Short.class);
		idTypes.add(Integer.class);
		idTypes.add(Long.class);
	}

	private final I id;
	private final Class<? extends IDataRecordIndex> type;
	private final String classifier;

	private byte[] byteRepresentative = null;
	private Integer hashCode = null;

	@SuppressWarnings("unchecked")
	public BitmapId(final byte[] bytes) {
		int offset = 0;

		final byte idTypePos = bytes[offset];
		offset++;

		final byte bitmapClassId = bytes[offset];
		offset++;

		// get the bitmapClass
		final Class<? extends IDataRecordIndex> bitmapClass = bitmapClasses
				.get((int) bitmapClassId);

		final byte lengthOfClassifiers = bytes[offset];
		offset++;

		// get the classifier
		final String classifier = Streams.byteToString(Arrays.copyOfRange(
				bytes, offset, offset + lengthOfClassifiers));
		offset += lengthOfClassifiers;

		// now get the id
		final Class<?> idType = idTypes.get(idTypePos);
		final I id;
		if (Byte.class.equals(idType)) {
			id = (I) new Byte(bytes[offset]);
		} else if (Short.class.equals(idType)) {
			final short idShort = Streams.byteToShort(Arrays.copyOfRange(bytes,
					offset, offset + 2));
			id = (I) new Short(idShort);
		} else if (Integer.class.equals(idType)) {
			final int idInt = Streams.byteToInt(Arrays.copyOfRange(bytes,
					offset, offset + 4));
			id = (I) new Integer(idInt);
		} else if (Long.class.equals(idType)) {
			final long idLong = Streams.byteToLong(Arrays.copyOfRange(bytes,
					offset, offset + 8));
			id = (I) new Long(idLong);
		} else {
			id = (I) Streams.byteToObject(Arrays.copyOfRange(bytes, offset,
					bytes.length));
		}

		// set the values
		this.byteRepresentative = bytes;
		this.id = id;
		this.type = bitmapClass;
		this.classifier = classifier.isEmpty() ? null : classifier;
	}

	public BitmapId(final I id, final Class<? extends IDataRecordIndex> type) {
		this(id, type, null);
	}

	public BitmapId(final I id, final Class<? extends IDataRecordIndex> type,
			final String classifier) {
		this.id = id;
		this.type = type;
		this.classifier = classifier == null || classifier.isEmpty() ? null
				: classifier;

		if (bitmapClasses.indexOf(getType()) == -1) {
			throw new IllegalArgumentException("The type '" + getType()
					+ "' is not supported.");
		}
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

	public String getClassifier() {
		if (classifier == null) {
			return "";
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

	@Override
	public boolean equals(final Object obj) {

		if (obj == this) {
			return true;
		} else if (obj instanceof BitmapId) {
			final BitmapId<?> bId = (BitmapId<?>) obj;

			return Objects.equals(bId.getId(), getId())
					&& Objects.equals(bId.getType(), getType())
					&& Objects.equals(bId.getClassifier(), getClassifier());
		} else {
			return false;
		}
	}

	public byte[] bytes() {

		if (byteRepresentative == null) {
			final int bitmapClassId = bitmapClasses.indexOf(getType());

			// get the idType number
			int idType = idTypes.indexOf(getIdType());
			if (idType == -1) {
				idType = Collections.getPosition(idTypes, Object.class);
			}

			// get the identifier
			final byte[] bytesIdType = new byte[] { (byte) idType };
			final byte[] bytesId;
			if (Byte.class.equals(getIdType())) {
				bytesId = new byte[] { (Byte) getId() };
			} else if (Short.class.equals(getIdType())) {
				bytesId = Streams.shortToByte((Short) getId());
			} else if (Integer.class.equals(getIdType())) {
				bytesId = Streams.intToByte((Integer) getId());
			} else if (Long.class.equals(getIdType())) {
				bytesId = Streams.longToByte((Long) getId());
			} else {
				bytesId = Streams.objectToByte(getId());
			}

			// get the class
			final byte[] bytesBitmapClass = new byte[] { (byte) bitmapClassId };

			// get the classifier
			final byte[] bytesClassifier = Streams
					.stringToByte(getClassifier());
			final byte[] bytesLength = new byte[] { (byte) bytesClassifier.length };

			// make sure the length is not invalid
			if (bytesClassifier.length > Byte.MAX_VALUE) {
				throw new IllegalArgumentException("The classifier '"
						+ classifier
						+ "' is to long and cannot be used for a bitmapId.");
			}

			// combine all the bytes
			byteRepresentative = Streams.combineBytes(bytesIdType,
					bytesBitmapClass, bytesLength, bytesClassifier, bytesId);
		}

		return byteRepresentative;
	}
}
