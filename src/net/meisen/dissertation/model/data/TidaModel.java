package net.meisen.dissertation.model.data;

import java.util.UUID;

import net.meisen.dissertation.config.xslt.DefaultValues;
import net.meisen.dissertation.model.indexes.datarecord.IntervalDataHandling;
import net.meisen.dissertation.model.indexes.datarecord.MetaDataHandling;
import net.meisen.dissertation.model.indexes.datarecord.TidaIndex;
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

		// add the data from the model
		this.idx.index(dataModel);

		// print the statistic after initialization
		if (LOG.isDebugEnabled()) {
			LOG.debug(idx.getStatistic());
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

	public MetaDataHandling getMetaDataHandling() {
		return metaDataHandling;
	}

	public void setMetaDataHandling(final MetaDataHandling metaDataHandling) {
		this.metaDataHandling = metaDataHandling == null ? MetaDataHandling
				.find(null) : metaDataHandling;

		// let the index know
		if (isInitialized()) {
			this.idx.setMetaDataHandling(this.metaDataHandling);
		}
	}

	public void setMetaDataHandlingByString(final String metaDataHandling) {
		setMetaDataHandling(MetaDataHandling.find(metaDataHandling));
	}

	public IntervalDataHandling getIntervalDataHandling() {
		return intervalDataHandling;
	}

	public void setIntervalDataHandling(final IntervalDataHandling handling) {
		this.intervalDataHandling = handling == null ? IntervalDataHandling
				.find(null) : handling;

		// let the index know
		if (isInitialized()) {
			this.idx.setIntervalDataHandling(this.intervalDataHandling);
		}
	}

	public void setIntervalDataHandlingByString(final String handling) {
		setIntervalDataHandling(IntervalDataHandling.find(handling));
	}
}
