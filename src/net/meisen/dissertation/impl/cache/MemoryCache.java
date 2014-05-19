package net.meisen.dissertation.impl.cache;

import java.io.File;

import net.meisen.dissertation.config.xslt.DefaultValues;
import net.meisen.dissertation.model.cache.IBitmapCache;
import net.meisen.dissertation.model.cache.IBitmapCacheConfig;
import net.meisen.dissertation.model.cache.IBitmapOwner;
import net.meisen.dissertation.model.indexes.BaseIndexFactory;
import net.meisen.dissertation.model.indexes.datarecord.slices.Bitmap;
import net.meisen.dissertation.model.indexes.datarecord.slices.BitmapId;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;

/**
 * The {@code MemoryCache} implementation is mainly used to create
 * {@code Bitmap} instances, when a bitmap is retrieved using the
 * {@link #getBitmap(BitmapId)} method. The cache expects that everything (i.e.
 * all bitmaps} is kept in-memory by the index structures. Therefore updates are
 * not recognized, nor any releases are published by this implementation.
 * 
 * @author pmeisen
 * 
 */
public class MemoryCache implements IBitmapCache {

	@Autowired
	@Qualifier(DefaultValues.INDEXFACTORY_ID)
	private BaseIndexFactory factory;

	@Override
	public void initialize(final String modelId, final File modelLocation) {
		// nothing to do
	}

	@Override
	public Bitmap getBitmap(final BitmapId<?> bitmapId) {
		return factory.createBitmap();
	}

	@Override
	public void cacheBitmap(final BitmapId<?> bitmapId, final Bitmap bitmap) {
		/*
		 * no interest the bitmap is already updated in memory
		 */
	}

	@Override
	public void registerBitmapOwner(final IBitmapOwner user) {
		/*
		 * the memory is not interested in any users, because nothing will ever
		 * be released
		 */
	}

	@Override
	public void setConfig(final IBitmapCacheConfig configuration) {
		// ignore there is no configuration
	}

	@Override
	public void release() {
		// nothing to be released
	}
}
