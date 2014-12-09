package net.meisen.dissertation.impl.cache;

import java.util.Collection;
import java.util.Collections;
import java.util.Iterator;

import net.meisen.dissertation.config.xslt.DefaultValues;
import net.meisen.dissertation.model.cache.IBitmapIdCache;
import net.meisen.dissertation.model.cache.IBitmapIdCacheConfig;
import net.meisen.dissertation.model.cache.IBitmapIdOwner;
import net.meisen.dissertation.model.cache.IReleaseMechanismCache;
import net.meisen.dissertation.model.data.TidaModel;
import net.meisen.dissertation.model.indexes.BaseIndexFactory;
import net.meisen.dissertation.model.indexes.datarecord.slices.Bitmap;
import net.meisen.dissertation.model.indexes.datarecord.slices.BitmapId;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;

/**
 * The {@code MemoryBitmapCache} implementation is mainly used to create
 * {@code Bitmap} instances, when a bitmap is retrieved using the
 * {@link #get(BitmapId)} method. The cache expects that everything (i.e. all
 * bitmaps} is kept in-memory by the index structures. Therefore updates are not
 * recognized, nor any releases are published by this implementation.
 * 
 * @author pmeisen
 * 
 */
public class MemoryBitmapCache implements IBitmapIdCache<Bitmap>,
		IReleaseMechanismCache<BitmapId<?>, Bitmap> {

	@Autowired
	@Qualifier(DefaultValues.INDEXFACTORY_ID)
	private BaseIndexFactory factory;

	@Override
	public void initialize(final TidaModel model) {
		// nothing to do
	}

	@Override
	public Bitmap get(final BitmapId<?> bitmapId) {
		return factory.createBitmap();
	}

	@Override
	public void cache(final BitmapId<?> bitmapId, final Bitmap bitmap) {
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
