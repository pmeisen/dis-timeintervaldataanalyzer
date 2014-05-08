package net.meisen.dissertation.model.data;

import java.io.InputStream;
import java.util.UUID;

import net.meisen.dissertation.config.xslt.DefaultValues;
import net.meisen.dissertation.exceptions.TidaModelException;
import net.meisen.dissertation.impl.cache.MemoryCache;
import net.meisen.dissertation.model.cache.IBitmapCache;
import net.meisen.dissertation.model.datasets.IClosableIterator;
import net.meisen.dissertation.model.datasets.IDataRecord;
import net.meisen.dissertation.model.indexes.BaseIndexFactory;
import net.meisen.dissertation.model.indexes.datarecord.IntervalDataHandling;
import net.meisen.dissertation.model.indexes.datarecord.MetaDataHandling;
import net.meisen.dissertation.model.indexes.datarecord.TidaIndex;
import net.meisen.dissertation.model.persistence.BasePersistor;
import net.meisen.dissertation.model.persistence.Group;
import net.meisen.dissertation.model.persistence.IPersistable;
import net.meisen.dissertation.model.persistence.Identifier;
import net.meisen.general.genmisc.exceptions.ForwardedRuntimeException;
import net.meisen.general.genmisc.exceptions.registry.IExceptionRegistry;

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
	@Qualifier(DefaultValues.CACHE_ID)
	private IBitmapCache cache;

	private final String id;
	private final String name;

	private TidaIndex idx;

	private MetaDataHandling metaDataHandling;
	private IntervalDataHandling intervalDataHandling;
	private OfflineMode offlineMode;

	private Group persistentGroup = null;

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
		this.id = id == null ? UUID.randomUUID().toString() : id;
		this.name = name == null ? id : name;

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
		return this.idx != null;
	}

	/**
	 * Initializes the model, i.e. get's it ready to work.
	 */
	public void initialize() {

		if (isInitialized()) {
			exceptionRegistry.throwException(TidaModelException.class, 1000);
		}

		// create the index
		this.idx = new TidaIndex(this);
	}

	/**
	 * Loads all the data specified by the {@code DataModel}.
	 */
	public void loadData() {

		if (!isInitialized()) {
			exceptionRegistry.throwException(TidaModelException.class, 1001,
					"loadData");
		} else if (LOG.isDebugEnabled()) {
			LOG.debug("Start adding of records from dataModel...");
		}

		// check the data and add it to the initialize index
		final IClosableIterator<IDataRecord> it = dataModel.iterator();
		int i = 0;

		try {
			while (it.hasNext()) {
				this.idx.index(it.next());

				if (++i % 10000 == 0 && LOG.isDebugEnabled()) {
					LOG.debug("... added " + i + " records from dataModel...");
				}
			}
		} catch (final RuntimeException e) {

			/*
			 * if the OfflineMode isn't enabled or if it's set to auto throw the
			 * exception, otherwise we just log it
			 */
			throw e;
		} finally {
			it.close();
		}

		// log the finalization
		if (LOG.isDebugEnabled()) {
			LOG.debug("Finished adding of " + i + " records from dataModel.");
		}

		// optimize the indexes after the loading
		this.idx.optimize();
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
		return idx.getAmountOfRecords();
	}

	@Override
	public void save(final BasePersistor persistor)
			throws ForwardedRuntimeException {
		// nothing to save
	}

	@Override
	public void load(final BasePersistor persistor,
			final Identifier identifier, final InputStream inputStream)
			throws ForwardedRuntimeException {
		// nothing to load
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

	public IBitmapCache getCache() {
		return cache;
	}
}
