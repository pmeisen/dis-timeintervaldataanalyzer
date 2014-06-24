package net.meisen.dissertation.impl.cache;

import net.meisen.dissertation.model.cache.IBitmapIdOwner;
import net.meisen.dissertation.model.cache.IFactDescriptorModelSetCache;
import net.meisen.dissertation.model.cache.IFactDescriptorModelSetCacheConfig;
import net.meisen.dissertation.model.cache.IReleaseMechanismCache;
import net.meisen.dissertation.model.data.TidaModel;
import net.meisen.dissertation.model.indexes.datarecord.slices.BitmapId;
import net.meisen.dissertation.model.indexes.datarecord.slices.FactDescriptorModelSet;

/**
 * A memory only version of a {@code FactDescriptorModelSetCache}.
 * 
 * @author pmeisen
 * 
 */
public class MemoryFactDescriptorModelSetCache implements
		IFactDescriptorModelSetCache,
		IReleaseMechanismCache<BitmapId<?>, FactDescriptorModelSet> {

	@Override
	public void initialize(final TidaModel model) {
		// nothing to do
	}

	@Override
	public FactDescriptorModelSet get(final BitmapId<?> bitmapId) {
		return new FactDescriptorModelSet();
	}

	@Override
	public void cacheFactDescriptorModelSet(final BitmapId<?> bitmapId,
			final FactDescriptorModelSet set) {
		/*
		 * no interest the bitmap is already updated in memory
		 */
	}

	@Override
	public void registerOwner(final IBitmapIdOwner user) {
		/*
		 * the memory is not interested in any users, because nothing will ever
		 * be released
		 */
	}

	@Override
	public void setConfig(IFactDescriptorModelSetCacheConfig configuration) {
		// ignore there is no configuration
	}

	@Override
	public void release() {
		// nothing to be released
	}

	@Override
	public boolean setPersistency(final boolean enable) {
		return false;
	}
}