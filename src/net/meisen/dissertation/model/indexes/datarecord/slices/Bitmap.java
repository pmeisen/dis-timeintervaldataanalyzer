package net.meisen.dissertation.model.indexes.datarecord.slices;

import java.io.DataInput;
import java.io.DataOutput;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import net.meisen.dissertation.impl.parser.query.select.evaluator.IBitmapResult;
import net.meisen.dissertation.model.cache.IBitmapIdCacheable;
import net.meisen.dissertation.model.indexes.BaseIndexFactory;
import net.meisen.dissertation.model.util.IIntIterator;

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
public abstract class Bitmap implements Iterable<Integer>, IBitmapContainer,
		IBitmapResult, IBitmapIdCacheable {

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
	 * Get the maximal identifier which can be set within the bitmap.
	 * 
	 * @return the maximal identifier which can be set within the bitmap
	 */
	public abstract int getMaxId();

	/**
	 * Gets the minimal identifier which can be set within the bitmap.
	 * 
	 * @return the minimal identifier which can be set within the bitmap
	 */
	public abstract int getMinId();

	/**
	 * Determines the cardinality of the bitmap.
	 * 
	 * @return the cardinality of the bitmap
	 */
	public abstract int determineCardinality();

	/**
	 * Checks if the bitmap has at least one bit set.
	 * 
	 * @return {@code true} if the bitmap has at least one bit set, otherwise
	 *         {@code false}
	 */
	public abstract boolean isBitSet();

	/**
	 * Optimizes the bitmap considering performance and storage.
	 */
	public abstract void optimize();

	/**
	 * Sets the specified identifiers of the {@code Bitmap} to {@code true}.
	 * Depending on the underlying implementation it has to be ensured, that the
	 * bitmap itself is not manipulated. It is ensured by the framework, that
	 * only one thread manipulates (e.g. calls set) on a bitmap, nevertheless
	 * the bitmap could be in use by several reading instances (i.e. select).
	 * Thereby, it might be wise to manipulate a clone and assign that to the
	 * internal bitmap after the manipulation.
	 * 
	 * @param recordIds
	 *            the identifiers to be set
	 */
	protected abstract void set(final int... recordIds);

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
	public abstract void serialize(final DataOutput out) throws IOException;

	/**
	 * Deserializes the bitmap from the specified {@code DataInputStream}.
	 * 
	 * @param in
	 *            the stream to deserialize the bitmap from
	 * 
	 * @throws IOException
	 *             if an exception using the input occurs
	 */
	protected abstract void deserialize(final DataInput in) throws IOException;

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
	 * Creates a new bitmap by combining {@code this} and the specified
	 * {@code bitmaps}.
	 * 
	 * @param bitmaps
	 *            the bitmaps to be combined with {@code this} using {@code XOR}
	 * @return the combined {@code Bitmap}
	 */
	public abstract Bitmap xor(final Bitmap... bitmaps);

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
	 * Creates a new instance of a {@code Bitmap} with the specified {@code ids}
	 * set.
	 * 
	 * @param factory
	 *            the {@code BaseIndexFactory} to create the {@code Bitmap}
	 *            instance
	 * @param ids
	 *            the identifiers to be set
	 * 
	 * @return the created {@code Bitmap}
	 */
	public static Bitmap createBitmap(final BaseIndexFactory factory,
			final int... ids) {
		final Bitmap bitmap = factory.createBitmap();
		bitmap.set(ids);

		return bitmap;
	}

	/**
	 * Creates a instance of a {@code Bitmap} based on the persisted version
	 * read from {@code in}.
	 * 
	 * @param factory
	 *            the {@code BaseIndexFactory} to create the {@code Bitmap}
	 *            instance
	 * @param in
	 *            the {@code DataInput} to read the persisted version from
	 * @return the read {@code Bitmap} instance
	 * 
	 * @throws IOException
	 *             if an error occurred during reading
	 */
	public static Bitmap createFromInput(final BaseIndexFactory factory,
			final DataInput in) throws IOException {
		final Bitmap bitmap = factory.createBitmap();
		bitmap.deserialize(in);

		return bitmap;
	}

	/**
	 * Gets an iterator used to iterate over the available integers set by
	 * {@code this}.
	 * 
	 * @return an iterator used to iterate over the available integers set by
	 *         {@code this}
	 */
	public abstract IIntIterator intIterator();

	/**
	 * Consider to use {@link #intIterator()} instead of {@code this}.
	 */
	@Override
	public Iterator<Integer> iterator() {
		return new Iterator<Integer>() {
			private final IIntIterator it = intIterator();

			@Override
			public boolean hasNext() {
				return it == null ? false : it.hasNext();
			}

			@Override
			public Integer next() {
				return it.next();
			}

			@Override
			public void remove() {
				throw new UnsupportedOperationException(
						"Remove is not supported.");
			}
		};
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

	/**
	 * Removes all the {@code null} values passed.
	 * 
	 * @param bitmaps
	 *            the bitmaps to remove {@code null} from
	 * 
	 * @return the none {@code null}-bitmaps
	 */
	public static Bitmap[] removeNulls(final Bitmap... bitmaps) {
		if (bitmaps == null || bitmaps.length == 0) {
			return new Bitmap[0];
		} else {
			final List<Bitmap> res = new ArrayList<Bitmap>(bitmaps.length);
			for (final Bitmap bitmap : bitmaps) {
				if (bitmap != null) {
					res.add(bitmap);
				}
			}

			return res.toArray(new Bitmap[0]);
		}
	}

	/**
	 * Combines the two bitmaps with each-other using an and-operation. A
	 * {@code null} as {@code timeBitmap} is handled as empty bitmap, whereby a
	 * {@code null} bitmap for the filter is handled as a full-bitmap, e.g. all
	 * values are set to one.
	 * 
	 * @param factory
	 *            the {@code IndexFactory} used if a new {@code Bitmap} has to
	 *            be created
	 * @param timeBitmap
	 *            the bitmap, specifying the selected values within the
	 *            time-dimension, {@code null} is handled as empty bitmap
	 * @param filter
	 *            specifying the filtered values, {@code null} is handled as
	 *            full bitmap
	 * 
	 * 
	 * @return the result of the combination
	 */
	public static Bitmap combineTimeAndFilter(final BaseIndexFactory factory,
			final Bitmap timeBitmap, final IBitmapResult filter) {
		if (timeBitmap == null) {
			return factory.createBitmap();
		} else if (filter == null || filter.getBitmap() == null) {
			return timeBitmap;
		} else {
			return filter.getBitmap().and(timeBitmap);
		}
	}

	/**
	 * Combines the bitmap with the result of the filter. A {@code null} value
	 * is handled as a full-bitmap, e.g. all values are set to one. If both
	 * bitmaps are {@code null}, {@code null} is returned.
	 * 
	 * @param bitmap
	 *            the bitmap used to combine with the filter
	 * @param filter
	 *            the filter
	 * 
	 * @return the result of the combination of both
	 */
	public static Bitmap combineBitmaps(final Bitmap bitmap,
			final IBitmapResult filter) {
		if (filter == null) {
			return bitmap;
		} else if (bitmap == null) {
			return filter.getBitmap();
		} else {
			return bitmap.and(filter.getBitmap());
		}
	}

	/**
	 * Combines the container and the bitmap with each-other using an
	 * or-operation. That said, if one is {@code null} the other is returned, if
	 * both are {@code null} {@code null} is returned, if both aren't null the
	 * or operation is applied.
	 * 
	 * @param bitmap
	 *            the bitmap
	 * @param container
	 *            the {@code BitmapContainer}
	 * 
	 * @return the result of or-combination, if both are {@code null},
	 *         {@code null} is returned
	 */
	public static Bitmap combineBitmaps(final Bitmap bitmap,
			final IBitmapContainer container) {
		if (container == null) {
			return bitmap;
		} else if (bitmap == null) {
			return container.getBitmap();
		} else {
			return bitmap.or(container.getBitmap());
		}
	}
}
