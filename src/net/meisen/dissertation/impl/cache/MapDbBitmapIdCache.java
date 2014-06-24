package net.meisen.dissertation.impl.cache;

import java.io.File;
import java.util.Collection;
import java.util.Map;

import net.meisen.dissertation.config.xslt.DefaultValues;
import net.meisen.dissertation.model.cache.IBitmapIdCache;
import net.meisen.dissertation.model.cache.IBitmapIdCacheConfig;
import net.meisen.dissertation.model.cache.IBitmapIdCacheable;
import net.meisen.dissertation.model.cache.IReferenceMechanismCache;
import net.meisen.dissertation.model.data.TidaModel;
import net.meisen.dissertation.model.indexes.datarecord.slices.BitmapId;
import net.meisen.general.genmisc.exceptions.registry.IExceptionRegistry;

import org.mapdb.DB;
import org.mapdb.DB.BTreeMapMaker;
import org.mapdb.DB.HTreeMapMaker;
import org.mapdb.DBMaker;
import org.mapdb.Serializer;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;

public abstract class MapDbBitmapIdCache<T extends IBitmapIdCacheable>
		implements IBitmapIdCache<T>, IReferenceMechanismCache<BitmapId<?>, T> {

	/**
	 * The {@code ExceptionRegistry}.
	 */
	@Autowired
	@Qualifier(DefaultValues.EXCEPTIONREGISTRY_ID)
	private IExceptionRegistry exceptionRegistry;

	private boolean init;
	private boolean persistency;

	private File location;
	private File modelLocation;
	private int cacheSize;
	private MapDbType type;

	private DB db;
	private Map<BitmapId<?>, T> map;

	public MapDbBitmapIdCache() {
		this.cacheSize = getDefaultCacheSize();

		this.init = false;
		this.persistency = true;

		// use the default configuration
		setConfig(null);
	}

	@Override
	public void initialize(final TidaModel model) {

		if (init) {
			return;
		}

		// read the values of the model needed
		final String modelId = model.getId();
		final File modelLocation = model.getLocation();

		// determine the location of the model
		if (this.location != null) {
			this.modelLocation = new File(location, modelId);
		} else if (modelLocation != null) {
			this.modelLocation = modelLocation;
			this.location = modelLocation.getParentFile();
		} else {
			this.location = getDefaultLocation();
			this.modelLocation = new File(getDefaultLocation(), modelId);
		}

		// create the location
		if (!this.modelLocation.exists() && !this.modelLocation.mkdirs()) {
			exceptionRegistry.throwException(MapDbBitmapIdCacheException.class,
					1004, modelLocation);
		}

		// initialize the maker to create the database
		final DBMaker<?> maker = DBMaker.newFileDB(new File(this.modelLocation,
				getIndexFileName()));
		maker.cacheHardRefEnable();
		maker.cacheLRUEnable();
		maker.cacheSize(cacheSize);
		maker.closeOnJvmShutdown();
		db = maker.make();

		// create the database and get the map
		final String mapName = getMapName();
		if (MapDbType.BTree.equals(type)) {
			final BTreeMapMaker mapMaker = db.createTreeMap(mapName);
			mapMaker.keySerializerWrap(new MapDbBitmapIdSerializer());
			mapMaker.valueSerializer(createValueSerializer());

			map = mapMaker.makeOrGet();
		} else if (MapDbType.HashMap.equals(type)) {
			final HTreeMapMaker mapMaker = db.createHashMap(mapName);
			mapMaker.keySerializer(new MapDbBitmapIdSerializer());
			mapMaker.valueSerializer(createValueSerializer());

			map = mapMaker.makeOrGet();
		} else {
			exceptionRegistry.throwException(MapDbBitmapIdCacheException.class,
					1002, type);
		}

		init = true;
	}

	protected abstract Serializer<T> createValueSerializer();

	/**
	 * Gets the name of the map to be used, should be unique.
	 * 
	 * @return the name of the map to be used
	 */
	protected String getMapName() {
		return getClass().getName();
	}

	/**
	 * Get the name of the index-file.
	 * 
	 * @return the name of the index-file
	 */
	protected abstract String getIndexFileName();

	/**
	 * Creates an empty, new instance of the {@code IBitmapIdCacheable}.
	 * 
	 * @return an empty, new instance of the {@code IBitmapIdCacheable}
	 */
	protected abstract T createNewInstance();

	/**
	 * Gets the location where the data of the cache is stored. This location is
	 * a sub-folder within the location specified by {@link #getLocation()}.
	 * 
	 * @return the location the cache stores data
	 */
	public File getModelLocation() {
		return this.modelLocation;
	}

	/**
	 * Gets the root-location of the caches. The {@code MapDbBitmapIdCache}
	 * generates a sub-folder within this {@code location}.
	 * 
	 * @return the root-location of the caches
	 */
	public File getLocation() {
		return this.location;
	}

	/**
	 * Gets the default-location used by the {@code MapDbBitmapIdCache} if no
	 * other is specified.
	 * 
	 * @return the default-location used by the {@code MapDbBitmapIdCache}
	 */
	protected File getDefaultLocation() {
		return new File(".");
	}

	/**
	 * Gets the default max size of the cache.
	 * 
	 * @return the default max size of the cache
	 */
	protected int getDefaultCacheSize() {
		return 100000;
	}

	/**
	 * Gets the default {@code MapDbType} to be used for the cache.
	 * 
	 * @return the default {@code MapDbType} to be used for the cache
	 */
	protected MapDbType getDefaultType() {
		return MapDbType.BTree;
	}

	@Override
	public T get(final BitmapId<?> id) {
		if (!isInit()) {
			exceptionRegistry.throwException(MapDbBitmapIdCacheException.class,
					1003);
		}

		// check if we have a result, if not create one
		final T res = map.get(id);
		if (res == null) {
			return createNewInstance();
		} else {
			return res;
		}
	}

	@Override
	public void setConfig(final IBitmapIdCacheConfig config) {
		if (init) {
			exceptionRegistry.throwException(MapDbBitmapIdCacheException.class,
					1000);
		} else if (config == null) {
			this.location = null;
			this.cacheSize = getDefaultCacheSize();
			this.type = getDefaultType();
		} else if (config instanceof MapDbBitmapIdCacheConfig) {
			final MapDbBitmapIdCacheConfig c = (MapDbBitmapIdCacheConfig) config;

			final File cLoc = c.getLocation();
			this.location = cLoc == null ? null : c.getLocation();

			final Integer cSize = c.getCacheSize();
			this.cacheSize = cSize == null ? getDefaultCacheSize() : cSize;
		} else {
			exceptionRegistry.throwException(MapDbBitmapIdCacheException.class,
					1001, config.getClass().getName());
		}
	}

	@Override
	public void release() {

		if (this.init) {

			// commit everything
			db.commit();

			// close the database
			db.close();
		}

		this.init = false;
	}

	@Override
	public synchronized boolean setPersistency(final boolean enable) {

		/*
		 * Make sure that there is no other thread working with the cache
		 * currently.
		 */
		final boolean oldPersistency = this.persistency;
		this.persistency = enable;

		// nothing to do, nothing was changed
		if (oldPersistency == this.persistency) {
			// nothing
		}
		/*
		 * Persistency was enabled and is disabled now.
		 */
		else if (oldPersistency) {
			// nothing to do
		}
		/*
		 * Persistency was disabled and is enabled now, write the not persisted
		 * once.
		 */
		else {
			commit();
		}

		return oldPersistency;
	}

	/**
	 * Checks if the persistency is currently enabled.
	 * 
	 * @return {@code true} if persistency is enabled, otherwise {@code false}
	 */
	public boolean isPersistencyEnabled() {
		return persistency;
	}

	@Override
	public boolean contains(final BitmapId<?> bitmapId) {
		if (!isInit()) {
			exceptionRegistry.throwException(MapDbBitmapIdCacheException.class,
					1003);
		}

		return map.containsKey(bitmapId);
	}

	@Override
	public void cache(final BitmapId<?> bitmapId, final T instance) {
		if (!isInit()) {
			exceptionRegistry.throwException(MapDbBitmapIdCacheException.class,
					1003);
		}

		map.put(bitmapId, instance);
		commit();
	}

	@Override
	public Collection<BitmapId<?>> getBitmapIdentifiers() {
		if (!isInit()) {
			exceptionRegistry.throwException(MapDbBitmapIdCacheException.class,
					1003);
		}

		return map.keySet();
	}

	protected void commit() {
		if (this.persistency) {
			db.commit();
		}
	}

	public boolean isInit() {
		return init;
	}
	
	public int size() {
		return map.size();
	}
}
