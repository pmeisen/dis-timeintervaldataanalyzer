package net.meisen.dissertation.model.data;

import java.util.UUID;

import net.meisen.dissertation.config.xslt.DefaultValues;
import net.meisen.dissertation.model.indexes.datarecord.IntervalDataHandling;
import net.meisen.dissertation.model.indexes.datarecord.MetaDataHandling;
import net.meisen.dissertation.model.indexes.datarecord.TidaIndex;
import net.meisen.dissertation.model.persistence.BasePersistor;
import net.meisen.dissertation.model.persistence.Group;
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
public class TidaModel {
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
	@Qualifier(DefaultValues.DATASTRUCTURE_ID)
	private DataStructure dataStructure;

	@Autowired
	@Qualifier(DefaultValues.persistor_ID)
	private BasePersistor persistor;

	private final String id;
	private final String name;

	private TidaIndex idx;

	private MetaDataHandling metaDataHandling;
	private IntervalDataHandling intervalDataHandling;

	/**
	 * Creates a {@code TimeIntervalDataAnalyzerModel} with a random id, the
	 * instance must be wired prior to it's usage to ensure that a
	 * {@code baseIndexedCollectionFactory} is available.
	 */
	public TidaModel() {
		this(null, null);
	}

	/**
	 * Creates a {@code TimeIntervalDataAnalyzerModel} with the specified
	 * {@code id}, the instance must be wired prior to it's usage to ensure that
	 * a {@code baseIndexedCollectionFactory} is available.
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
	 * prior to it's usage to ensure that a {@code baseIndexedCollectionFactory}
	 * is available.
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
		setMetaDataHandling((MetaDataHandling) null);
		setIntervalDataHandling((IntervalDataHandling) null);
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

	public void validate() {

	}

	public boolean isInitialized() {
		return this.idx != null;
	}

	public void initialize() {

		if (isInitialized()) {
			// TODO add exception already initialized
		}

		// create the index
		this.idx = new TidaIndex(this);

		// register the entities which might want to persist data
		persistor.register(new Group("indexes"), this.idx);
	}

	/**
	 * Loads all the data specified by the {@code DataModel}.
	 */
	public void loadData() {
		this.idx.index(dataModel);
		
		// optimize the indexes after the loading
		this.idx.optimize();

		// print the statistic after data loading
		if (LOG.isDebugEnabled()) {
			LOG.debug(idx.toStatistic());
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

		// let the index know
		if (isInitialized()) {
			this.idx.setMetaDataHandling(this.metaDataHandling);
		}
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

		// let the index know
		if (isInitialized()) {
			this.idx.setIntervalDataHandling(this.intervalDataHandling);
		}
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

	public void save(final String location) {
		if (!isInitialized()) {
			// TODO add exception not initialized
		}

		// save the data to the specified location
		persistor.save(location);
	}

	/**
	 * Get the instance defined to persist data.
	 * 
	 * @return the instance defined to persist data
	 */
	public BasePersistor getPersistor() {
		return persistor;
	}
}
