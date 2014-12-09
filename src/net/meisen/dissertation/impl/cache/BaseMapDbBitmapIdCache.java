package net.meisen.dissertation.impl.cache;

import java.util.Collection;
import java.util.Iterator;

import org.mapdb.Serializer;

import net.meisen.dissertation.model.cache.IBitmapIdCache;
import net.meisen.dissertation.model.cache.IBitmapIdCacheConfig;
import net.meisen.dissertation.model.cache.IBitmapIdCacheable;
import net.meisen.dissertation.model.indexes.datarecord.slices.BitmapId;

/**
 * Base implementation of a {@code mapDb} used to associate an instance to a
 * {@code BitmapId}.
 * 
 * @author pmeisen
 * 
 * @param <T>
 *            the type of the instance to be associated
 */
public abstract class BaseMapDbBitmapIdCache<T extends IBitmapIdCacheable>
		extends BaseMapDbCache<BitmapId<?>, T> implements IBitmapIdCache<T> {

	@Override
	public void setConfig(final IBitmapIdCacheConfig config) {
		if (config == null || config instanceof MapDbCacheConfig) {
			super.setConfig((MapDbCacheConfig) config);
		} else {
			exceptionRegistry.throwException(BaseMapDbCacheException.class,
					1001, config.getClass().getName());
		}
	}

	/**
	 * Gets a collection of all the cached {@code BitmapId} instances.
	 * 
	 * @return a collection of all the cached {@code BitmapId} instances
	 */
	public Collection<BitmapId<?>> getBitmapIdentifiers() {
		return getIdentifiers();
	}
	
	@Override
	public Iterator<BitmapId<?>> iterator() {
		return getBitmapIdentifiers().iterator();
	}

	@Override
	protected MapDbType getDefaultType() {
		return MapDbType.BTree;
	}

	@Override
	protected Serializer<BitmapId<?>> createKeySerializer() {
		return new MapDbBitmapIdSerializer();
	}
}
