package net.meisen.dissertation.impl.cache;

import net.meisen.dissertation.model.cache.IIdentifierCacheConfig;
import net.meisen.dissertation.model.indexes.datarecord.slices.Bitmap;

/**
 * A {@code MemoryIdentifierCache} is used to cache information about the
 * identifiers used to identify the different data internally in memory only.
 * 
 * @author pmeisen
 * 
 */
public class MemoryIdentifierCache extends BaseIdentifierCache {

	@Override
	protected void cacheBitmap(final Bitmap newBitmap) {
		// nothing to do
	}

	@Override
	protected void cacheIdentifier(final int lastUsedIdentifier) {
		// nothing to do
	}

	@Override
	public void setConfig(final IIdentifierCacheConfig config) {
		// nothing to be configured
	}
	
	@Override
	public void remove() {
		// nothing to do
	}
}
