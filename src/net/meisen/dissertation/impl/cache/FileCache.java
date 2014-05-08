package net.meisen.dissertation.impl.cache;

import java.io.File;

import net.meisen.dissertation.config.xslt.DefaultValues;
import net.meisen.dissertation.model.cache.IBitmapCache;
import net.meisen.dissertation.model.cache.IBitmapCacheConfig;
import net.meisen.dissertation.model.cache.IBitmapUser;
import net.meisen.dissertation.model.indexes.BaseIndexFactory;
import net.meisen.dissertation.model.indexes.datarecord.slices.Bitmap;
import net.meisen.dissertation.model.indexes.datarecord.slices.BitmapId;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;

public class FileCache implements IBitmapCache {

	@Autowired
	@Qualifier(DefaultValues.INDEXFACTORY_ID)
	private BaseIndexFactory factory;

	private File location;

	public FileCache() {
		this.location = getDefaultLocation();
	}

	@Override
	public Bitmap getBitmap(final BitmapId<?> bitmapId) {
		return factory.createBitmap();
	}

	@Override
	public void cacheBitmap(final BitmapId<?> bitmapId, final Bitmap bitmap) {

	}

	@Override
	public void registerBitmapUser(final IBitmapUser user) {

	}
	
	public File getLocation() {
		return this.location;
	}

	@Override
	public void setConfig(final IBitmapCacheConfig config) {
		if (config == null) {
			this.location = getDefaultLocation();
		} else if (config instanceof FileCacheConfig) {
			final FileCacheConfig fcConfig = (FileCacheConfig) config;
			this.location = fcConfig.getLocation();
		} else {

			// TODO LOG A WARNING

			this.location = getDefaultLocation();
		}
	}

	protected File getDefaultLocation() {
		return new File(".");
	}
}
