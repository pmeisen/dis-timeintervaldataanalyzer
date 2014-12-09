package net.meisen.dissertation.impl.cache;

import java.util.Collection;
import java.util.Collections;
import java.util.Iterator;

import net.meisen.dissertation.model.cache.IBitmapIdCache;
import net.meisen.dissertation.model.cache.IBitmapIdCacheConfig;
import net.meisen.dissertation.model.cache.IBitmapIdOwner;
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
		IBitmapIdCache<FactDescriptorModelSet>,
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
	public void cache(final BitmapId<?> bitmapId,
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
	public void setConfig(final IBitmapIdCacheConfig configuration) {
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

	@Override
	public boolean contains(BitmapId<?> bitmapId) {
		return false;
	}

	/**
	 * Gets a collection of all the cached {@code BitmapId} instances.
	 * 
	 * @return a collection of all the cached {@code BitmapId} instances
	 */
	public Collection<BitmapId<?>> getBitmapIdentifiers() {
		return Collections.emptyList();
	}
	
	@Override
	public Iterator<BitmapId<?>> iterator() {
		return getBitmapIdentifiers().iterator();
	}
	
	@Override
	public void remove() {
		// nothing to do
	}
}