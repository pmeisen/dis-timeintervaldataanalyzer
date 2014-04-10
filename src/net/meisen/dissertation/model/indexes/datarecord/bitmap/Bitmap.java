package net.meisen.dissertation.model.indexes.datarecord.bitmap;

import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

import net.meisen.dissertation.model.indexes.BaseIndexFactory;

/**
 * A wrapper for a {@code Bitmap} implementation, e.g.
 * {@code EWAHCompressedBitmap}. The implementation must provide a default
 * constructor, otherwise the creation by the factory might fail.
 * 
 * @author pmeisen
 * 
 * @see BaseIndexFactory
 * 
 */
public abstract class Bitmap implements IBitmapContainer {

	/**
	 * The logical types to combine a bitmap with another.
	 * 
	 * @author pmeisen
	 * 
	 */
	protected static enum LogicType {
		/**
		 * The two bitmaps A and B will be combined using a logical {@code AND}.
		 */
		AND,
		/**
		 * The two bitmaps A and B will be combined using a logical {@code OR}.
		 */
		OR;
	}

	/**
	 * Get the set identifiers for the bitmap.
	 * 
	 * @return the identifiers set
	 */
	public abstract int[] getIds();

	/**
	 * Determines the cardinality of the bitmap.
	 * 
	 * @return the cardinality of the bitmap
	 */
	public abstract int determineCardinality();

	/**
	 * Optimizes the bitmap considering performance and storage.
	 */
	public abstract void optimize();

	/**
	 * Sets the specified identifiers of the {@code Bitmap} to {@code true}.
	 * 
	 * @param recordIds
	 *            the identifiers to be set
	 */
	public abstract void set(final int... recordIds);

	/**
	 * Creates a deep-copy of the {@code Bitmap}.
	 * 
	 * @return a deep-copy of the {@code Bitmap}
	 */
	public abstract Bitmap copy();

	/**
	 * Serializes the bitmap to the specified {@code DataOutputStream}.
	 * 
	 * @param out
	 *            the stream to serialize the bitmap to
	 * 
	 * @throws IOException
	 *             if an exception using the output occurs
	 */
	public abstract void serialize(final DataOutputStream out)
			throws IOException;

	/**
	 * Deserializes the bitmap from the specified {@code DataInputStream}.
	 * 
	 * @param in
	 *            the stream to deserialize the bitmap from
	 * 
	 * @throws IOException
	 *             if an exception using the input occurs
	 */
	public abstract void deserialize(final DataInputStream in)
			throws IOException;

	/**
	 * Inverts the bitmap up to the specified {@code position}, i.e.
	 * {@code 0 - position}.
	 * 
	 * @param position
	 *            the position to invert the bitmaps up to
	 * 
	 * @return the inverted bitmap
	 */
	public abstract Bitmap invert(final int position);

	/**
	 * Determines the cardinality of the inverted bitmap up to the specified
	 * {@code position}, i.e. {@code 0 - position}.
	 * 
	 * @param position
	 *            the position to invert the bitmaps up to
	 * 
	 * @return the cardinality of the inverted {@code Bitmap}
	 */
	public abstract int invertCardinality(final int position);

	/**
	 * Creates a new bitmap by combining {@code this} and the specified
	 * {@code bitmaps}.
	 * 
	 * @param bitmaps
	 *            the bitmaps to be combined with {@code this} using {@code AND}
	 * @return the combined {@code Bitmap}
	 */
	public abstract Bitmap and(final Bitmap... bitmaps);

	/**
	 * Calculates the cardinality of the bitmap created by combining
	 * {@code this} and the specified {@code bitmaps}.
	 * 
	 * @param bitmaps
	 *            the bitmaps to be combined with {@code this} using {@code AND}
	 * 
	 * @return the cardinality of the combined {@code Bitmap}
	 */
	public abstract int andCardinality(final Bitmap... bitmaps);

	/**
	 * Creates a new bitmap by combining {@code this} and the specified
	 * {@code bitmaps}.
	 * 
	 * @param bitmaps
	 *            the bitmaps to be combined with {@code this} using {@code OR}
	 * @return the combined {@code Bitmap}
	 */
	public abstract Bitmap or(final Bitmap... bitmaps);

	/**
	 * Calculates the cardinality of the bitmap created by combining
	 * {@code this} and the specified {@code bitmaps}.
	 * 
	 * @param bitmaps
	 *            the bitmaps to be combined with {@code this} using {@code OR}
	 * 
	 * @return the cardinality of the combined {@code Bitmap}
	 */
	public abstract int orCardinality(final Bitmap... bitmaps);

	@Override
	public Bitmap getBitmap() {
		return this;
	}

	/**
	 * Combines the specified bitmaps using a logical {@code AND}.
	 * 
	 * @param factory
	 *            the {@code IndexFactory} used if a new {@code Bitmap} has to
	 *            be created
	 * @param bitmaps
	 *            the bitmaps to combine
	 * 
	 * @return the combined result
	 */
	public static Bitmap and(final BaseIndexFactory factory,
			final Object... bitmaps) {
		return combine(bitmaps, LogicType.AND, factory);
	}

	/**
	 * Combines the specified bitmaps using a logical {@code OR}.
	 * 
	 * @param factory
	 *            the {@code IndexFactory} used if a new {@code Bitmap} has to
	 *            be created
	 * @param bitmaps
	 *            the bitmaps to combine
	 * 
	 * @return the combined result
	 */
	public static Bitmap or(final BaseIndexFactory factory,
			final Object... bitmaps) {
		return combine(bitmaps, LogicType.OR, factory);
	}

	/**
	 * Helper method used to combine the specified {@code bitmaps} with each
	 * other using the specified logical {@code type}. If the array of
	 * {@code bitmaps} contains a {@code null} it is assumed that this
	 * represents an empty bitmap, i.e. no value is set to {@code true}.
	 * 
	 * @param bitmaps
	 *            the array of bitmaps to be combined
	 * @param type
	 *            the {@code LogicType} used to combine the {@code bitmaps}
	 * @param factory
	 *            the {@code IndexFactory} used if a new {@code Bitmap} has to
	 *            be created
	 * 
	 * @return the resulting combined {@code Bitmap}
	 * 
	 * @see LogicType
	 */
	protected static Bitmap combine(final Object[] bitmaps,
			final LogicType type, final BaseIndexFactory factory) {
		final List<Bitmap> others = new ArrayList<Bitmap>();
		Bitmap first = null;

		// check if there are bitmaps
		if (bitmaps != null && bitmaps.length > 0) {
			for (final Object o : bitmaps) {
				final Bitmap bitmap = determineBitmap(o);

				if (bitmap == null) {

					/*
					 * depending on the type we already now the result if null
					 * is handled as 0-bitmap
					 */
					if (LogicType.AND.equals(type)) {
						first = null;
						break;
					}
					continue;
				} else {

					/*
					 * we have a bitmap, so if it's the first one keep it,
					 * otherwise add it to the array
					 */
					if (first == null) {
						first = bitmap;
					} else {
						others.add(bitmap);
					}
				}
			}
		}

		/*
		 * if we don't have any first one create the array
		 */
		if (first == null) {
			return factory.createBitmap();
		} else if (others.size() == 0) {
			return first;
		} else if (LogicType.AND.equals(type)) {
			return first.and(others.toArray(new Bitmap[0]));
		} else if (LogicType.OR.equals(type)) {
			return first.or(others.toArray(new Bitmap[0]));
		} else {
			throw new IllegalStateException("Using unsupported LogicType '"
					+ type + "'.");
		}
	}

	/**
	 * Helper method used to determine the {@code Bitmap} of the specified
	 * {@code o}.
	 * 
	 * @param o
	 *            the object to determine the {@code Bitmap} from
	 * @return the determined {@code Bitmap}, or {@code null} if none could be
	 *         determined
	 * 
	 * @see Bitmap
	 * @see IBitmapContainer
	 */
	protected static Bitmap determineBitmap(final Object o) {
		if (o instanceof Bitmap) {
			return (Bitmap) o;
		} else if (o instanceof IBitmapContainer) {
			return ((IBitmapContainer) o).getBitmap();
		} else {
			return null;
		}
	}
}
