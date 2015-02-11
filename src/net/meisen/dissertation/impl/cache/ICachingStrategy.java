package net.meisen.dissertation.impl.cache;

import java.util.List;
import java.util.Set;

import net.meisen.dissertation.model.indexes.datarecord.slices.BitmapId;

/**
 * A strategy to be used.
 * 
 * @author pmeisen
 * 
 */
public interface ICachingStrategy {

	/**
	 * Triggers the strategy to consider the usage of a {@code Bitmap}.
	 * 
	 * @param bitmapId
	 *            the identifier of the used {@code Bitmap}
	 */
	public void usedBitmap(final BitmapId<?> bitmapId);

	/**
	 * Registers a bitmap to be part of the {@code CachingStrategy}. The bitmap
	 * is not assumed to be used, i.e. all counters should be set to {@code 0}.
	 * 
	 * @param bitmapId
	 *            the bitmap to be registered
	 */
	public void registerBitmap(final BitmapId<?> bitmapId);

	/**
	 * Determines the less used {@code Bitmap} instances.
	 * 
	 * @param set
	 * 
	 * @param amount
	 *            the amount of items to be retrieved
	 * @param update
	 *            {@code true} if the strategy should be updated, i.e. the
	 *            elements are removed from the strategy measures, otherwise
	 *            {@code false}
	 * 
	 * @return the elements to be removed sorted from head to tail (remove to do
	 *         not remove)
	 */
	public List<BitmapId<?>> determineRemovables(final Set<BitmapId<?>> set,
			final int amount, final boolean update);

}
