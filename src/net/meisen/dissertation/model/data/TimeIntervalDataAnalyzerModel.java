package net.meisen.dissertation.model.data;

import java.util.UUID;

import net.meisen.dissertation.config.xslt.DefaultValues;
import net.meisen.general.genmisc.exceptions.registry.IExceptionRegistry;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;

public class TimeIntervalDataAnalyzerModel {

	@Autowired
	@Qualifier(DefaultValues.EXCEPTIONREGISTRY_ID)
	private IExceptionRegistry exceptionRegistry;

	@Autowired
	@Qualifier(DefaultValues.METADATAMODEL_ID)
	private MetaDataModel metaDataModel;

	@Autowired
	@Qualifier(DefaultValues.DATAMODEL_ID)
	private DataModel dataModel;

	private final String id;
	private final String name;

	/**
	 * Creates a {@code TimeIntervalDataAnalyzerModel} with a random id, the
	 * instance must be wired prior to it's usage to ensure that a
	 * {@code baseIndexedCollectionFactory} is available.
	 */
	public TimeIntervalDataAnalyzerModel() {
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
	public TimeIntervalDataAnalyzerModel(final String id) {
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
	public TimeIntervalDataAnalyzerModel(final String id, final String name) {
		this.id = id == null ? UUID.randomUUID().toString() : id;
		this.name = name == null ? id : name;
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
}
