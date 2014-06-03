package net.meisen.dissertation.model.data;

import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.util.Iterator;
import java.util.UUID;
import java.util.concurrent.locks.ReentrantReadWriteLock;

import net.meisen.dissertation.config.xslt.DefaultValues;
import net.meisen.dissertation.exceptions.PersistorException;
import net.meisen.dissertation.exceptions.TidaModelException;
import net.meisen.dissertation.model.cache.IBitmapCache;
import net.meisen.dissertation.model.cache.IFactDescriptorModelSetCache;
import net.meisen.dissertation.model.cache.IIdentifierCache;
import net.meisen.dissertation.model.cache.IMetaDataCache;
import net.meisen.dissertation.model.data.metadata.MetaDataCollection;
import net.meisen.dissertation.model.datasets.IClosableIterator;
import net.meisen.dissertation.model.datasets.IDataRecord;
import net.meisen.dissertation.model.indexes.BaseIndexFactory;
import net.meisen.dissertation.model.indexes.datarecord.IntervalDataHandling;
import net.meisen.dissertation.model.indexes.datarecord.MetaDataHandling;
import net.meisen.dissertation.model.indexes.datarecord.TidaIndex;
import net.meisen.dissertation.model.indexes.datarecord.slices.Bitmap;
import net.meisen.dissertation.model.persistence.BasePersistor;
import net.meisen.dissertation.model.persistence.Group;
import net.meisen.dissertation.model.persistence.IPersistable;
import net.meisen.dissertation.model.persistence.Identifier;
import net.meisen.general.genmisc.exceptions.ForwardedRuntimeException;
import net.meisen.general.genmisc.exceptions.registry.IExceptionRegistry;
import net.meisen.general.genmisc.types.Files;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;

/**
 * A {@code TidaModel} is used to combine the {@code MetaDataModel}, the
 * {@code DataModel} and the {@code DataStructure}.
 * 
 * @author pmeisen
 * 
 */
public class TidaModel implements IPersistable {
	private final static String EXTENSION = ".model";
	private final static Logger LOG = LoggerFactory.getLogger(TidaModel.class);

	@Autowired
	@Qualifier(DefaultValues.EXCEPTIONREGISTRY_ID)
	private IExceptionRegistry exceptionRegistry;

	@Autowired
	@Qualifier(DefaultValues.METADATAMODEL_ID)
	private MetaDataModel metaDataModel;

	@Autowired
	@Qualifier(DefaultValues.DATAMODEL_ID)
	private DataModel dataModel;

	@Autowired
	@Qualifier(DefaultValues.INTERVALMODEL_ID)
	private IntervalModel intervalModel;

	@Autowired
	@Qualifier(DefaultValues.INDEXFACTORY_ID)
	private BaseIndexFactory indexFactory;

	@Autowired
	@Qualifier(DefaultValues.DATASTRUCTURE_ID)
	private DataStructure dataStructure;

	@Autowired
	@Qualifier(DefaultValues.IDENTIFIERCACHE_ID)
	private IIdentifierCache idCache;

	@Autowired
	@Qualifier(DefaultValues.BITMAPCACHE_ID)
	private IBitmapCache bitmapCache;

	@Autowired
	@Qualifier(DefaultValues.FACTSETSCACHE_ID)
	private IFactDescriptorModelSetCache factsCache;

	@Autowired
	@Qualifier(DefaultValues.METADATACACHE_ID)
	private IMetaDataCache metaDataCache;

	private final String id;
	private final String name;
	private final File location;

	private boolean initialized;

	private TidaIndex idx;

	private MetaDataHandling metaDataHandling;
	private IntervalDataHandling intervalDataHandling;
	private OfflineMode offlineMode;

	private Group persistentGroup = null;

	private ReentrantReadWriteLock loadLock = new ReentrantReadWriteLock();

	/**
	 * Creates a {@code TimeIntervalDataAnalyzerModel} with a random id, the
	 * instance must be wired prior to it's usage to ensure that an
	 * {@code IndexFactory} is available.
	 */
	public TidaModel() {
		this(null, null);
	}

	/**
	 * Creates a {@code TimeIntervalDataAnalyzerModel} with the specified
	 * {@code id}, the instance must be wired prior to it's usage to ensure that
	 * an {@code IndexFactory}
	 * 
	 * @param id
	 *            the identifier used for the {@code MetaDataModel}
	 */
	public TidaModel(final String id) {
		this(id, null);
	}

	/**
	 * Creates a {@code TimeIntervalDataAnalyzerModel} with the specified
	 * {@code id} and the specified {@code name}, the instance must be wired
	 * prior to it's usage to ensure that an {@code IndexFactory} is available.
	 * 
	 * @param id
	 *            the identifier used for the {@code MetaDataModel}
	 * @param name
	 *            the name of the {@code MetaDataModel}, if {@code null} the
	 *            name will be equal to the {@code id}
	 */
	public TidaModel(final String id, final String name) {
		this(id, name, null);
	}

	/**
	 * /** Creates a {@code TimeIntervalDataAnalyzerModel} with the specified
	 * {@code id} and the specified {@code name}, the instance must be wired
	 * prior to it's usage to ensure that an {@code IndexFactory} is available.
	 * 
	 * @param id
	 *            the identifier used for the {@code MetaDataModel}
	 * @param name
	 *            the name of the {@code MetaDataModel}, if {@code null} the
	 *            name will be equal to the {@code id}
	 * @param location
	 *            the working directory of the model
	 */
	public TidaModel(final String id, final String name, final File location) {
		this.id = id == null ? UUID.randomUUID().toString() : id;
		this.name = name == null ? id : name;
		this.location = location == null ? new File(".", this.id) : location;

		this.initialized = false;

		// set the default value
		setMetaDataHandling(null);
		setIntervalDataHandling(null);
		setOfflineMode(null);
	}

	/**
	 * Gets the identifier of the {@code TimeIntervalDataAnalyzerModel}.
	 * 
	 * @return the identifier of the {@code TimeIntervalDataAnalyzerModel}
	 */
	public String getId() {
		return id;
	}

	/**
	 * Gets the name of the {@code TimeIntervalDataAnalyzerModel}.
	 * 
	 * @return the name of the {@code TimeIntervalDataAnalyzerModel}
	 */
	public String getName() {
		return name;
	}

	/**
	 * Checks if the {@code Model} is initialized.
	 * 
	 * @return {@code true} if it is initialized, otherwise {@code false}.
	 */
	public boolean isInitialized() {
		return initialized;
	}

	/**
	 * Initializes the model, i.e. get's it ready to work.
	 */
	public void initialize() {

		// do not allow to initialize the cache twice
		if (isInitialized()) {
			exceptionRegistry.throwException(TidaModelException.class, 1000);
		}

		// make sure the location exists
		final File loc = getLocation();
		final boolean locExists = loc.exists();
		if (!locExists && !loc.mkdirs()) {
			exceptionRegistry.throwException(TidaModelException.class, 1002,
					loc);
		} else if (locExists && !loc.isDirectory()) {
			exceptionRegistry.throwException(TidaModelException.class, 1003,
					loc);
		}

		// initialize the caches
		this.metaDataCache.initialize(this);
		getIdentifierCache().initialize(this);
		getBitmapCache().initialize(this);
		getFactsCache().initialize(this);

		/*
		 * Get the cached metaData and use it. Afterwards the data is stored in
		 * the MetaDataModel and not needed anymore, therefore release the
		 * cache. Before releasing the cache is used to persist the currently
		 * loaded metaData.
		 */
		final MetaDataCollection metaData = this.metaDataCache
				.createMetaDataCollection();
		this.metaDataModel.addMetaData(metaData);
		this.metaDataCache.cacheMetaDataModel(this.metaDataModel);
		this.metaDataCache.release();
		this.metaDataCache = null;

		// create the index
		this.idx = new TidaIndex(this, getIdentifierCache()
				.getLastUsedIdentifier());

		// log the initialization
		if (LOG.isInfoEnabled()) {
			LOG.info("Initialized model '" + getId() + "' at '" + getLocation()
					+ "'.");
		}

		this.initialized = true;
	}

	/**
	 * Release the model and all the resources bound to it.
	 * 
	 * @param deleteLocation
	 *            {@code true} if all folders should be deleted, which were
	 *            created by the model, otherwise {@code false}
	 */
	public void release(final boolean deleteLocation) {

		// log the release
		if (isInitialized() && LOG.isInfoEnabled()) {
			LOG.info("Releasing model '" + getId() + "' at '" + getLocation()
					+ ".");
		}

		// release the cache
		getBitmapCache().release();
		getFactsCache().release();
		getIdentifierCache().release();

		// delete created files if necessary
		if (deleteLocation && getLocation().exists()) {
			if (LOG.isDebugEnabled()) {
				LOG.debug("Deleting the files of the model '" + getId()
						+ "' at '" + getLocation() + ".");
			}

			if (!Files.deleteDir(getLocation())) {
				exceptionRegistry.throwException(TidaModelException.class,
						1004, getLocation());
			}
		}

		// the model isn't initialized anymore
		initialized = false;
	}

	/**
	 * Releases all the resources
	 */
	public void release() {
		release(false);
	}

	/**
	 * Loads the specified record into the database.
	 * 
	 * @param record
	 *            the {@code DataRecord} to be loaded
	 */
	public void loadRecord(final IDataRecord record) {
		loadRecord(getDataStructure(), record);
	}

	/**
	 * Loads the specified record into the database.
	 * 
	 * @param dataStructure
	 *            the {@code DataStructure} to be used for the data
	 * @param record
	 *            the {@code DataRecord} to be loaded
	 */
	public void loadRecord(final DataStructure dataStructure,
			final IDataRecord record) {

		// make sure the model is initialized
		if (!isInitialized()) {
			exceptionRegistry.throwException(TidaModelException.class, 1001,
					"loadData");
		}

		loadLock.writeLock().lock();
		try {
			_loadRecord(dataStructure, record);
		} finally {
			loadLock.writeLock().unlock();
		}
	}

	/**
	 * Loads the specified record into the database without checking any
	 * preconditions.
	 * 
	 * @param dataStructure
	 *            the {@code DataStructure} to be used for the data
	 * @param record
	 *            the {@code DataRecord} to be loaded
	 */
	protected void _loadRecord(final DataStructure dataStructure,
			final IDataRecord record) {

		// get the identifier which will be used for the next record
		final int id = this.idx.getNextDataId();

		// persist the identifier as used
		getIdentifierCache().markIdentifierAsUsed(id);

		// now index the record
		this.idx.index(dataStructure, record);

		// set the bitmap for the specified identifier
		getIdentifierCache().markIdentifierAsValid(id);
	}

	/**
	 * Loads the specified data in a bulk-load, i.e. the data is not persisted
	 * until all data is read.
	 * 
	 * @param it
	 *            the {@code iterator} for the data to be loaded
	 * 
	 * @return the amount of data loaded
	 */
	public int bulkLoadData(final Iterator<IDataRecord> it) {
		return bulkLoadData(getDataStructure(), it);
	}

	/**
	 * Loads the specified data in a bulk-load, i.e. the data is not persisted
	 * until all data is read.
	 * 
	 * @param dataStructure
	 *            the {@code DataStructure} to be used for the data
	 * @param it
	 *            the {@code iterator} for the data to be loaded
	 * 
	 * @return the amount of data loaded
	 */
	public int bulkLoadData(final DataStructure dataStructure,
			final Iterator<IDataRecord> it) {

		// make sure the model is initialized
		if (!isInitialized()) {
			exceptionRegistry.throwException(TidaModelException.class, 1001,
					"loadData");
		}

		if (LOG.isDebugEnabled()) {
			LOG.debug("Start adding of records...");
		}

		// inform the cache about bulk-loads
		loadLock.writeLock().lock();
		int amountOfData = 0;
		boolean oldPersistencyBitmap = true;
		boolean oldPersistencyFacts = true;
		try {
			oldPersistencyBitmap = getBitmapCache().setPersistency(false);
			oldPersistencyFacts = getFactsCache().setPersistency(false);

			try {
				while (it.hasNext()) {
					_loadRecord(dataStructure, it.next());

					if (++amountOfData % 10000 == 0 && LOG.isDebugEnabled()) {
						LOG.debug("... added " + amountOfData + " records...");
					}
				}
			} catch (final RuntimeException e) {
				throw e;
			} finally {
				if (it instanceof IClosableIterator) {
					((IClosableIterator<?>) it).close();
				}
			}
		} finally {

			// enable the persistency again, everything is loaded
			getBitmapCache().setPersistency(oldPersistencyBitmap);
			getFactsCache().setPersistency(oldPersistencyFacts);

			loadLock.writeLock().unlock();
		}

		// log the finalization
		if (LOG.isDebugEnabled()) {
			LOG.debug("Finished adding of " + amountOfData + " records.");
		}

		// optimize the indexes after the loading
		this.idx.optimize();

		// return the amount of data written
		return amountOfData;
	}

	/**
	 * Loads all the data specified by the {@code DataModel}.
	 */
	public void bulkLoadDataFromDataModel() {

		// make sure the model is initialized
		if (!isInitialized()) {
			exceptionRegistry.throwException(TidaModelException.class, 1001,
					"loadData");
		}

		// check the data and add it to the initialize index
		final IClosableIterator<IDataRecord> it = dataModel.iterator();
		try {
			bulkLoadData(it);
		} catch (final RuntimeException e) {
			throw e;
		} finally {
			it.close();
		}
	}

	/**
	 * Gets the {@code MetaDataModel} of the {@code TidaModel}.
	 * 
	 * @return the {@code MetaDataModel} of the {@code TidaModel}
	 */
	public MetaDataModel getMetaDataModel() {
		return metaDataModel;
	}

	/**
	 * Gets the {@code DataModel} of the {@code TidaModel}.
	 * 
	 * @return the {@code DataModel} of the {@code TidaModel}
	 */
	public DataModel getDataModel() {
		return dataModel;
	}

	/**
	 * Gets the {@code IntervalModel} of the {@code TidaModel}.
	 * 
	 * @return the {@code IntervalModel} of the {@code TidaModel}
	 */
	public IntervalModel getIntervalModel() {
		return intervalModel;
	}

	/**
	 * Gets the {@code DataStructure} of the {@code TidaModel}.
	 * 
	 * @return the {@code DataStructure} of the {@code TidaModel}
	 */
	public DataStructure getDataStructure() {
		return dataStructure;
	}

	/**
	 * Gets the handling used for meta-data, i.e. if the meta-data found in the
	 * data cannot be mapped to any defined meta-data.
	 * 
	 * @return the handling used for meta-data
	 * 
	 * @see MetaDataHandling
	 */
	public MetaDataHandling getMetaDataHandling() {
		return metaDataHandling;
	}

	/**
	 * Sets the handling of the meta-data.
	 * 
	 * @param metaDataHandling
	 *            the handling of the meta-data
	 * 
	 * @see MetaDataHandling
	 */
	public void setMetaDataHandling(final MetaDataHandling metaDataHandling) {
		this.metaDataHandling = metaDataHandling == null ? MetaDataHandling
				.find(null) : metaDataHandling;
	}

	/**
	 * Sets the handling of the meta-data.
	 * 
	 * @param metaDataHandling
	 *            the handling of the meta-data
	 * 
	 * @see MetaDataHandling
	 */
	public void setMetaDataHandlingByString(final String metaDataHandling) {
		setMetaDataHandling(MetaDataHandling.find(metaDataHandling));
	}

	/**
	 * Gets the current setting of the {@code OfflineMode}.
	 * 
	 * @return the settings of the {@code OfflineMode}
	 * 
	 * @see OfflineMode
	 */
	public OfflineMode getOfflineMode() {
		return offlineMode;
	}

	/**
	 * Sets the {@code OfflineMode}, i.e. how invalid data retrievers should be
	 * handled.
	 * 
	 * @param mode
	 *            the {@code OfflineMode} to be used
	 * 
	 * @see IntervalDataHandling
	 */
	public void setOfflineMode(final OfflineMode mode) {
		this.offlineMode = mode == null ? OfflineMode.find(null) : mode;

		final MetaDataModel metaDataModel = getMetaDataModel();
		if (metaDataModel != null) {
			metaDataModel.setOfflineMode(this.offlineMode);
		}
		if (dataModel != null) {
			dataModel.setOfflineMode(this.offlineMode);
		}
	}

	/**
	 * Sets the {@code OfflineMode}, i.e. how invalid data retrievers should be
	 * handled.
	 * 
	 * @param mode
	 *            the {@code OfflineMode} to be used
	 * 
	 * @see IntervalDataHandling
	 */
	public void setOfflineModeByString(final String mode) {
		setOfflineMode(OfflineMode.find(mode));
	}

	/**
	 * Gets the handling of the interval-data, i.e. if {@code null} values are
	 * found.
	 * 
	 * @return the handling of the interval-data
	 * 
	 * @see IntervalDataHandling
	 */
	public IntervalDataHandling getIntervalDataHandling() {
		return intervalDataHandling;
	}

	/**
	 * Sets the handling of the interval-data, i.e. if {@code null} values are
	 * found.
	 * 
	 * @param handling
	 *            the handling of the interval-data
	 * 
	 * @see IntervalDataHandling
	 */
	public void setIntervalDataHandling(final IntervalDataHandling handling) {
		this.intervalDataHandling = handling == null ? IntervalDataHandling
				.find(null) : handling;
	}

	/**
	 * Sets the handling of the interval-data, i.e. if {@code null} values are
	 * found.
	 * 
	 * @param handling
	 *            the handling of the interval-data
	 * 
	 * @see IntervalDataHandling
	 */
	public void setIntervalDataHandlingByString(final String handling) {
		setIntervalDataHandling(IntervalDataHandling.find(handling));
	}

	/**
	 * Gets the index of the model.
	 * 
	 * @return the used index of the model
	 */
	public TidaIndex getIndex() {
		return idx;
	}

	/**
	 * Get the next identifier used for a record.
	 * 
	 * @return the next identifier used for a record
	 */
	public int getNextDataId() {
		if (!isInitialized()) {
			exceptionRegistry.throwException(TidaModelException.class, 1001,
					"nextRecordId");
		}

		return idx.getNextDataId();
	}

	/**
	 * Get the amount of stored records.
	 * 
	 * @return the amount of stored records
	 */
	public int getAmountOfRecords() {
		return getIdentifierCache().getValidIdentifiers()
				.determineCardinality();
	}

	@Override
	public void isRegistered(final BasePersistor persistor, final Group group) {
		if (!isInitialized()) {
			exceptionRegistry.throwException(TidaModelException.class, 1001,
					"persistorRegistration");
		}

		this.persistentGroup = group;

		persistor.register(group.append("index"), this.idx);
	}

	@Override
	public void save(final BasePersistor persistor)
			throws ForwardedRuntimeException {
		final IIdentifierCache cache = getIdentifierCache();
		final Identifier identifier = new Identifier(id + EXTENSION,
				persistentGroup);

		final OutputStream out = persistor.openForWrite(identifier);

		try {
			persistor.writeInt(out, cache.getLastUsedIdentifier());
			cache.getValidIdentifiers().serialize(new DataOutputStream(out));
		} catch (final IOException e) {
			throw new ForwardedRuntimeException(PersistorException.class, 1003,
					e, e.getMessage());
		} finally {
			persistor.close(identifier);
		}
	}

	@Override
	public void load(final BasePersistor persistor,
			final Identifier identifier, final InputStream inputStream)
			throws ForwardedRuntimeException {
		final IIdentifierCache cache = getIdentifierCache();

		// get the InputStream
		try {
			cache.markIdentifierAsUsed(persistor.readInt(inputStream));
			cache.markIdentifierAsValid(Bitmap.createFromInput(indexFactory,
					new DataInputStream(inputStream)));
		} catch (final Exception e) {
			throw new ForwardedRuntimeException(PersistorException.class, 1004,
					e, e.getMessage());
		}
	}

	@Override
	public Group getPersistentGroup() {
		return persistentGroup;
	}

	/**
	 * Get the {@code IndexFactory} used by the model.
	 * 
	 * @return the {@code IndexFactory} used by the model
	 */
	public BaseIndexFactory getIndexFactory() {
		return indexFactory;
	}

	/**
	 * Gets the {@code IdentifierCache} used by the model.
	 * 
	 * @return the {@code IdentifierCache} used by the model
	 * 
	 * @see IIdentifierCache
	 */
	public IIdentifierCache getIdentifierCache() {
		return idCache;
	}

	/**
	 * Gets the {@code BitmapCache} used by the model.
	 * 
	 * @return the {@code BitmapCache} used by the model
	 * 
	 * @see IBitmapCache
	 */
	public IBitmapCache getBitmapCache() {
		return bitmapCache;
	}

	/**
	 * Gets the {@code FactDescriptorModelSetCache} used by the model.
	 * 
	 * @return the {@code FactDescriptorModelSetCache} used by the model
	 * 
	 * @see IFactDescriptorModelSetCache
	 */
	public IFactDescriptorModelSetCache getFactsCache() {
		return factsCache;
	}

	/**
	 * The location to store the model's data.
	 * 
	 * @return the model's location
	 */
	public File getLocation() {
		return location;
	}

	/**
	 * Gets a bitmap identifying the valid records.
	 * 
	 * @return a bitmap identifying the valid records
	 */
	public Bitmap getValidRecords() {
		return getIdentifierCache().getValidIdentifiers();
	}
}
