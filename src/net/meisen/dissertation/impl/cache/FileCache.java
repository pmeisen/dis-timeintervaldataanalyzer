package net.meisen.dissertation.impl.cache;

import java.io.File;
import java.util.HashMap;
import java.util.Map;

import net.meisen.dissertation.config.xslt.DefaultValues;
import net.meisen.dissertation.exceptions.FileCacheException;
import net.meisen.dissertation.model.cache.IBitmapCache;
import net.meisen.dissertation.model.cache.IBitmapCacheConfig;
import net.meisen.dissertation.model.cache.IBitmapUser;
import net.meisen.dissertation.model.indexes.BaseIndexFactory;
import net.meisen.dissertation.model.indexes.datarecord.slices.Bitmap;
import net.meisen.dissertation.model.indexes.datarecord.slices.BitmapId;
import net.meisen.general.genmisc.exceptions.registry.IExceptionRegistry;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;

public class FileCache implements IBitmapCache {
	private final Map<BitmapId<?>, IBitmapUser> users;

	@Autowired
	@Qualifier(DefaultValues.EXCEPTIONREGISTRY_ID)
	private IExceptionRegistry exceptionRegistry;

	@Autowired
	@Qualifier(DefaultValues.INDEXFACTORY_ID)
	private BaseIndexFactory factory;

	private boolean init;
	private File location;

	public FileCache() {
		this.users = new HashMap<BitmapId<?>, IBitmapUser>();

		this.location = getDefaultLocation();
		this.init = false;
	}

	protected void initialize() {

		// if already initialized we are done
		if (this.init) {
			return;
		}

		this.init = true;
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
		if (init) {
			exceptionRegistry.throwException(FileCacheException.class, 1000);
		} else if (config == null) {
			this.location = getDefaultLocation();
		} else if (config instanceof FileCacheConfig) {
			final FileCacheConfig fcConfig = (FileCacheConfig) config;
			this.location = fcConfig.getLocation();
		} else {
			exceptionRegistry.throwException(FileCacheException.class, 1001,
					config.getClass().getName());
		}
	}

	protected File getDefaultLocation() {
		return new File(".");
	}
}
