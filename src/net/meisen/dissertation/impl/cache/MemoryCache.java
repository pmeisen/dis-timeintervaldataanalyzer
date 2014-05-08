package net.meisen.dissertation.impl.cache;

import net.meisen.dissertation.config.xslt.DefaultValues;
import net.meisen.dissertation.model.cache.IBitmapCache;
import net.meisen.dissertation.model.cache.IBitmapCacheConfig;
import net.meisen.dissertation.model.cache.IBitmapUser;
import net.meisen.dissertation.model.indexes.BaseIndexFactory;
import net.meisen.dissertation.model.indexes.datarecord.slices.Bitmap;
import net.meisen.dissertation.model.indexes.datarecord.slices.BitmapId;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;

public class MemoryCache implements IBitmapCache {

	@Autowired
	@Qualifier(DefaultValues.INDEXFACTORY_ID)
	private BaseIndexFactory factory;

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
	public void registerBitmapUser(final IBitmapUser user) {
		/*
		 * the memory is not interested in any users, because nothing will ever
		 * be released
		 */
	}

	@Override
	public void setConfig(final IBitmapCacheConfig configuration) {
		// ignore there is no configuration
	}
}
