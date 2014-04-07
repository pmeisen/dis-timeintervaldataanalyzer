package net.meisen.dissertation.model.indexes.datarecord.bitmap;

import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

import net.meisen.dissertation.model.indexes.BaseIndexedCollectionFactory;

/**
 * A wrapper for a {@code Bitmap} implementation, e.g.
 * {@code EWAHCompressedBitmap}. The implementation must provide a default
 * constructor, otherwise the creation by the factory fails.
 * 
 * @author pmeisen
 * 
 */
public abstract class Bitmap {

	protected static enum LogicType {
		AND, OR;
	}

	public abstract int[] getIds();

	public abstract int determineCardinality();

	public abstract void optimize();

	public abstract void set(final int... recordIds);

	public abstract void serialize(final DataOutputStream out)
			throws IOException;

	public abstract void deserialize(final DataInputStream in)
			throws IOException;

	public abstract Bitmap and(final Bitmap... bitmaps);

	public abstract int andCardinality(final Bitmap... bitmaps);

	public abstract Bitmap or(final Bitmap... bitmaps);

	public abstract int orCardinality(final Bitmap... bitmaps);

	public static Bitmap and(final BaseIndexedCollectionFactory factory,
			final Object... bitmaps) {
		return combine(bitmaps, LogicType.AND, factory);
	}

	public static Bitmap or(final BaseIndexedCollectionFactory factory,
			final Object... bitmaps) {
		return combine(bitmaps, LogicType.OR, factory);
	}

	protected static Bitmap combine(final Object[] bitmaps,
			final LogicType type, final BaseIndexedCollectionFactory factory) {
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
