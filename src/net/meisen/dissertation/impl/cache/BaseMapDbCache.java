package net.meisen.dissertation.impl.cache;

import java.io.File;
import java.util.Collection;
import java.util.Iterator;
import java.util.Map;
import java.util.SortedMap;

import net.meisen.dissertation.config.xslt.DefaultValues;
import net.meisen.dissertation.model.cache.ICache;
import net.meisen.dissertation.model.cache.IReferenceMechanismCache;
import net.meisen.dissertation.model.data.TidaModel;
import net.meisen.general.genmisc.exceptions.ForwardedRuntimeException;
import net.meisen.general.genmisc.exceptions.registry.IExceptionRegistry;
import net.meisen.general.genmisc.types.Files;

import org.mapdb.DB;
import org.mapdb.DB.BTreeMapMaker;
import org.mapdb.DB.HTreeMapMaker;
import org.mapdb.DBMaker;
import org.mapdb.Serializer;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;

/**
 * A base implementation for a cache using the {@code mapDb}.
 * 
 * @author pmeisen
 * 
 * @param <K>
 *            the key of the map
 * @param <T>
 *            the value of the map
 */
public abstract class BaseMapDbCache<K, T> implements ICache,
		IReferenceMechanismCache<K, T> {
	private final static Logger LOG = LoggerFactory
			.getLogger(BaseMapDbCache.class);

	/**
	 * The {@code ExceptionRegistry}.
	 */
	@Autowired
	@Qualifier(DefaultValues.EXCEPTIONREGISTRY_ID)
	protected IExceptionRegistry exceptionRegistry;

	private boolean init;
	private boolean persistency;

	private File location;
	private File modelLocation;
	private int cacheSize;
	private MapDbType type;

	private DB db;
	private Map<K, T> map;

	/**
	 * Default constructor.
	 */
	public BaseMapDbCache() {
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
			exceptionRegistry.throwException(BaseMapDbCacheException.class,
					1004, modelLocation);
		}

		// initialize the maker to create the database
		final DBMaker<?> maker = DBMaker.newFileDB(new File(this.modelLocation,
				getIndexFileName()));
		modifyDbMaker(maker);
		db = maker.make();

		// create the database and get the map
		final String mapName = getMapName();
		if (MapDbType.BTree.equals(type)) {
			final BTreeMapMaker mapMaker = db.createTreeMap(mapName);
			final Serializer<K> keySer = createKeySerializer();
			final Serializer<T> valueSer = getValueSerializer(db, model);
			if (keySer != null) {
				mapMaker.keySerializerWrap(keySer);
			}
			if (valueSer != null) {
				mapMaker.valueSerializer(valueSer);
			}

			try {
				map = mapMaker.makeOrGet();
			} catch (final AssertionError e) {
				exceptionRegistry.throwException(BaseMapDbCacheException.class,
						1005, modelLocation, e.getMessage());
			}
		} else if (MapDbType.HashMap.equals(type)) {
			final HTreeMapMaker mapMaker = db.createHashMap(mapName);
			final Serializer<K> keySer = createKeySerializer();
			final Serializer<T> valueSer = getValueSerializer(db, model);
			if (keySer != null) {
				mapMaker.keySerializer(keySer);
			}
			if (valueSer != null) {
				mapMaker.valueSerializer(valueSer);
			}

			map = mapMaker.makeOrGet();
		} else {
			exceptionRegistry.throwException(BaseMapDbCacheException.class,
					1002, type);
		}

		init = true;
	}

	/**
	 * Method called directly after the {@code maker} was created. Concrete
	 * implementations can be used to modify the {@code maker}.
	 * 
	 * @param maker
	 *            the created {@code DBMaker}
	 * 
	 * @see DBMaker
	 */
	protected void modifyDbMaker(final DBMaker<?> maker) {
		if (isInit()) {
			exceptionRegistry.throwException(BaseMapDbCacheException.class,
					1000);
		}

		maker.cacheHardRefEnable();
		maker.cacheLRUEnable();
		maker.cacheSize(getCacheSize());
		maker.closeOnJvmShutdown();
	}

	@Override
	public void release() {

		if (isInit()) {
			if (LOG.isDebugEnabled()) {
				LOG.debug("Releasing MapDbCache at '" + modelLocation + "' ("
						+ getClass().getSimpleName() + ").");
			}

			// commit everything
			db.commit();

			// close the database
			db.close();
		}

		this.init = false;
	}

	/**
	 * Sets the configuration for the {@code mapDb} cache.
	 * 
	 * @param config
	 *            the configuration for the cache
	 */
	public void setConfig(final MapDbCacheConfig config) {
		if (isInit()) {
			exceptionRegistry.throwException(BaseMapDbCacheException.class,
					1000);
		} else if (config == null) {
			this.location = null;
			this.cacheSize = getDefaultCacheSize();
			this.type = getDefaultType();
		} else {
			final MapDbCacheConfig c = (MapDbCacheConfig) config;

			final File cLoc = c.getLocation();
			this.location = cLoc == null ? null : c.getLocation();

			final Integer cSize = c.getCacheSize();
			this.cacheSize = cSize == null ? getDefaultCacheSize() : cSize;
		}
	}

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
	 * Gets the defined size for the cache.
	 * 
	 * @return the defined size for the cache
	 */
	public int getCacheSize() {
		return cacheSize;
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
	protected abstract MapDbType getDefaultType();

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

	/**
	 * Checks if the {@code mapDb} and thereby the cache contains the
	 * {@code key}.
	 * 
	 * @param key
	 *            the key to be checked
	 * @return {@code true} if it is contained, otherwise {@code false}
	 */
	public boolean contains(final K key) {
		if (!isInit()) {
			exceptionRegistry.throwException(BaseMapDbCacheException.class,
					1003);
		}

		return map.containsKey(key);
	}

	/**
	 * Commits the current data if persistancy is disabled.
	 */
	protected void commit() {
		if (this.persistency) {
			db.commit();
		}
	}

	/**
	 * Checks if the cache is initialized.
	 * 
	 * @return {@code true} if initialized, otherwise {@code false}
	 */
	public boolean isInit() {
		return init;
	}

	/**
	 * Get the amount of elements currently cached.
	 * 
	 * @return the amount of elements currently cached
	 */
	public int size() {
		return map.size();
	}

	/**
	 * Caches the specified {@code instance} under the specified {@code key}.
	 * 
	 * @param key
	 *            the key to specify the {@code instance} under
	 * @param instance
	 *            the instance to be cached
	 */
	public void cache(final K key, final T instance) {
		if (!isInit()) {
			exceptionRegistry.throwException(BaseMapDbCacheException.class,
					1003);
		}

		map.put(key, instance);
		commit();
	}

	/**
	 * Gets all the identifiers available.
	 * 
	 * @return all the available identifiers
	 */
	public Collection<K> getIdentifiers() {
		if (!isInit()) {
			exceptionRegistry.throwException(BaseMapDbCacheException.class,
					1003);
		}

		return map.keySet();
	}

	/**
	 * Creates an empty, new instance of the {@code IBitmapIdCacheable}. The
	 * newly created instance is not cached, because it is assumed that it will
	 * be created the same way every time. If the value should be cached, the
	 * using instance should cache it.
	 * 
	 * @return an empty, new instance of the {@code IBitmapIdCacheable}, can be
	 *         {@code null}
	 */
	protected abstract T createNewInstance();

	/**
	 * Gets the {@code ValueSerializer}. The {@code Serializer} is created using
	 * {@link #createValueSerializer()} if not found in the catalog of the
	 * database. If the {@code Serializer} is of type
	 * {@code IModelDependendMapDbSerializer} it is additionally initialized.
	 * 
	 * @param db
	 *            the database, can be {@code null}
	 * @param model
	 *            the model needed for initialization
	 * 
	 * @return the created or retrieved and initialized {@code Serializer}
	 */
	@SuppressWarnings("unchecked")
	protected Serializer<T> getValueSerializer(final DB db,
			final TidaModel model) {

		// get the Serializer of the db
		Serializer<T> serializer = null;
		if (db != null) {
			final SortedMap<String, Object> catalog = db.getCatalog();
			if (catalog != null) {
				serializer = (Serializer<T>) catalog.get(getMapName()
						+ ".valueSerializer");
			}
		}

		// if we still don't have any Serializer create one
		if (serializer == null) {
			serializer = createValueSerializer();
		}

		// initialize the Serializer if it's one that can be initialized
		if (serializer instanceof IModelDependendMapDbSerializer) {
			try {
				((IModelDependendMapDbSerializer<T>) serializer).init(this,
						model);
			} catch (final ForwardedRuntimeException e) {
				exceptionRegistry.throwRuntimeException(e);
			}
		}

		return serializer;
	}

	/**
	 * Method is called to create an instance of a {@code Serializer} used to
	 * serialize the value stored in the {@code mapDb}.
	 * 
	 * @return the created {@code Serializer}, might be {@code null} if no
	 *         special {@code Serializer} is needed
	 */
	protected abstract Serializer<T> createValueSerializer();

	/**
	 * Method is called to create an instance of a {@code Serializer} used to
	 * serialize the key stored in the {@code mapDb}.
	 * 
	 * @return the created {@code Serializer}, might be {@code null} if no
	 *         special {@code Serializer} is needed
	 */
	protected abstract Serializer<K> createKeySerializer();

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

	@Override
	public T get(final K key) {
		if (!isInit()) {
			exceptionRegistry.throwException(BaseMapDbCacheException.class,
					1003);
		}

		// check if we have a result, if not create one
		final T res = map.get(key);
		if (res == null) {
			return createNewInstance();
		} else {
			return res;
		}
	}

	/**
	 * Gets an iterator to iterate over the keys of the {@code map}.
	 * 
	 * @return an iterator to iterate over the keys
	 */
	public Iterator<K> keyIterator() {
		return map.keySet().iterator();
	}

	@Override
	public void remove() {
		if (isInit()) {
			exceptionRegistry.throwException(BaseMapDbCacheException.class,
					1006);
		}

		if (!Files.deleteOnExitDir(getModelLocation()) && LOG.isErrorEnabled()) {
			LOG.error("Unable to delete the files created for the cache '"
					+ getClass().getSimpleName() + "' at '"
					+ Files.getCanonicalPath(getModelLocation()) + "'");
		}
	}
}
