package net.meisen.dissertation.model.indexes.datarecord.slices;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import net.meisen.dissertation.model.indexes.datarecord.IDataRecordIndex;
import net.meisen.dissertation.model.indexes.datarecord.IntervalIndex;
import net.meisen.dissertation.model.indexes.datarecord.MetaIndex;
import net.meisen.general.genmisc.types.Numbers;
import net.meisen.general.genmisc.types.Objects;
import net.meisen.general.genmisc.types.Streams;
import net.meisen.general.genmisc.types.Streams.ByteResult;
import net.meisen.general.genmisc.types.Strings;

/**
 * A {@code BitmapId} is used to identify a bitmap within a model.
 * 
 * @author pmeisen
 * 
 * @param <I>
 *            the type of the identifier used within the {@code BitmapId}
 */
public class BitmapId<I> implements Comparable<BitmapId<I>> {

	private final static List<Class<? extends IDataRecordIndex>> bitmapClasses;
	static {
		bitmapClasses = new ArrayList<Class<? extends IDataRecordIndex>>(2);

		bitmapClasses.add(MetaIndex.class);
		bitmapClasses.add(IntervalIndex.class);
	}

	private final I id;
	private final Class<? extends IDataRecordIndex> type;
	private final String classifier;

	private Integer hashCode = null;

	/**
	 * Constructor to create a {@code BitmapId} based on a byte-representation.
	 * 
	 * @param bytes
	 *            the byte-representation to be used
	 * 
	 * @see #bytes()
	 */
	@SuppressWarnings("unchecked")
	public BitmapId(final byte[] bytes) {
		ByteResult res;

		// get the type
		this.type = bitmapClasses.get(bytes[0]);

		// get the identifier
		res = Streams.byteToObject(bytes, 1);
		this.id = (I) res.object;

		// get the classifier
		res = Streams.byteToObject(bytes, res.nextPos);
		this.classifier = (String) res.object;
	}

	/**
	 * Constructor to create a {@code BitmapId} without any classifier.
	 * 
	 * @param id
	 *            the identifier
	 * @param type
	 *            the type
	 */
	public BitmapId(final I id, final Class<? extends IDataRecordIndex> type) {
		this(id, type, null);
	}

	/**
	 * The constructor used to define a {@code BitmapId} with {@code classifier}
	 * .
	 * 
	 * @param id
	 *            the identifier
	 * @param type
	 *            the type
	 * @param classifier
	 *            the classifier
	 */
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

	/**
	 * Gets the type of the identifier defined for the {@code BitmapId}.
	 * 
	 * @return the type of the identifier
	 */
	@SuppressWarnings("unchecked")
	public Class<I> getIdType() {
		return (Class<I>) getId().getClass();
	}

	/**
	 * Gets the identifier defined for the {@code BitmapId}.
	 * 
	 * @return the identifier
	 */
	public I getId() {
		return id;
	}

	/**
	 * Gets the type defined for the {@code BitmapId}.
	 * 
	 * @return the type
	 */
	public Class<? extends IDataRecordIndex> getType() {
		return type;
	}

	/**
	 * Gets the classifier defined for the {@code BitmapId}.
	 * 
	 * @return the classifier
	 */
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
			return getId() + " (" + getType().getSimpleName() + ")";
		} else {
			return c + "." + getId() + " (" + getType().getSimpleName() + ")";
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

			return bId.hashCode() == hashCode()
					&& Objects.equals(bId.getId(), getId())
					&& Objects.equals(bId.getType(), getType())
					&& Objects.equals(bId.getClassifier(), getClassifier());
		} else {
			return false;
		}
	}

	/**
	 * Gets a byte-representation of the {@code BitmapId}. The representation
	 * cannot exceed the size defined by {@link #getMaxBytesLength()}.
	 * 
	 * @return the byte-representation of the {@code BitmapId}
	 * 
	 * @see BitmapId#createByteRepresentation()
	 */
	public final byte[] bytes() {

		final byte[] result = createByteRepresentation();

		// make sure the size is ok
		if (result.length > getMaxBytesLength()) {
			throw new IllegalArgumentException(
					"The byte-representation exceeds the maximal possible size of a bitmapId.");
		}

		return result;
	}

	/**
	 * Creates the byte-representation of {@code this}. The representation
	 * cannot exceed the size defined by {@link #getMaxBytesLength()}. The size
	 * is validated within {@link #bytes()}.
	 * 
	 * @return the byte-representation of {@code this}
	 */
	protected byte[] createByteRepresentation() {
		final byte bitmapClassId = Numbers.castToByte(bitmapClasses
				.indexOf(getType()));

		// get the different representations
		final byte[] bytesClassId = new byte[] { bitmapClassId };
		final byte[] bytesId = Streams.objectToByte(id);
		final byte[] bytesClassifier = Streams.objectToByte(classifier);

		// make sure the length is not invalid
		if (bytesClassifier.length > Byte.MAX_VALUE) {
			throw new IllegalArgumentException("The classifier '" + classifier
					+ "' is too long and cannot be used for a bitmapId.");
		} else if (bytesId.length > Byte.MAX_VALUE) {
			throw new IllegalArgumentException("The identifier '" + getId()
					+ "' is too long (" + bytesId.length + ">" + Byte.MAX_VALUE
					+ " ).");
		}

		return Streams.combineBytes(bytesClassId, bytesId, bytesClassifier);
	}

	/**
	 * Gets the maximal size of a byte representation of a {@code BitmapId}.
	 * 
	 * @return the maximal length of bytes
	 */
	public static int getMaxBytesLength() {
		/*
		 * The calculation is:
		 * 
		 * bytesBitmapClass (1) + bytesId (Byte.MAX_VALUE) + bytesClassifier
		 * (Byte.MAX_VALUE)
		 */
		return 1 + Byte.MAX_VALUE + Byte.MAX_VALUE;
	}

	@Override
	public int compareTo(final BitmapId<I> bitmapId) {

		// compare each attribute
		int cmp = Objects.compare(getId(), bitmapId.getId());
		if (cmp == 0) {
			cmp = Objects.compare(getType(), bitmapId.getType());
			if (cmp == 0) {
				cmp = Objects
						.compare(getClassifier(), bitmapId.getClassifier());
			}
		}

		return cmp;
	}
}
