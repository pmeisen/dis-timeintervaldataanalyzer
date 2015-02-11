package net.meisen.dissertation.impl.cache;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.Set;

import net.meisen.dissertation.model.indexes.datarecord.slices.BitmapId;

/**
 * A caching strategy removing (not really random, but somehow random) elements.
 * 
 * @author pmeisen
 * 
 */
public class RandomCachingStrategy implements ICachingStrategy {

	@Override
	public void usedBitmap(final BitmapId<?> bitmapId) {
		// do nothing
	}

	@Override
	public void registerBitmap(final BitmapId<?> bitmapId) {
		// do nothing
	}

	@Override
	public List<BitmapId<?>> determineRemovables(final Set<BitmapId<?>> set,
			final int amount, final boolean update) {

		final List<BitmapId<?>> removables = new ArrayList<BitmapId<?>>();

		final Iterator<BitmapId<?>> it = set.iterator();
		int i = 0;
		while (i < amount && it.hasNext()) {
			removables.add(it.next());
			i++;
		}

		return removables;
	}
}
