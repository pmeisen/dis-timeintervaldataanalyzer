package net.meisen.dissertation.model.data;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.Map.Entry;

import net.meisen.dissertation.config.xslt.DefaultValues;
import net.meisen.dissertation.exceptions.MetaDataModelException;
import net.meisen.dissertation.model.data.metadata.IIdentifiedMetaData;
import net.meisen.dissertation.model.data.metadata.IMetaData;
import net.meisen.dissertation.model.data.metadata.IMetaDataCollection;
import net.meisen.dissertation.model.data.metadata.IOfflineModeAwareMetaData;
import net.meisen.dissertation.model.descriptors.Descriptor;
import net.meisen.dissertation.model.descriptors.DescriptorModel;
import net.meisen.dissertation.model.indexes.BaseIndexFactory;
import net.meisen.dissertation.model.indexes.IIndexedCollection;
import net.meisen.dissertation.model.indexes.IndexKeyDefinition;
import net.meisen.dissertation.model.indexes.datarecord.IntervalDataHandling;
import net.meisen.dissertation.model.indexes.datarecord.MetaDataHandling;
import net.meisen.general.genmisc.exceptions.registry.IExceptionRegistry;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;

/**
 * The model which keeps all the {@code MetaData} of the data used for the
 * analyzes. This {@code MetaData} includes all descriptions about the data. The
 * model adds {@code Indexes} to support a fast access.
 * 
 * @author pmeisen
 * 
 */
public class MetaDataModel {
	private final static Logger LOG = LoggerFactory
			.getLogger(MetaDataModel.class);

	@Autowired
	@Qualifier(DefaultValues.EXCEPTIONREGISTRY_ID)
	private IExceptionRegistry exceptionRegistry;

	@Autowired
	@Qualifier(DefaultValues.INDEXFACTORY_ID)
	private BaseIndexFactory indexFactory;

	private IIndexedCollection descriptorModels;
	private OfflineMode offlineMode;

	/**
	 * Creates a {@code MetaDataModel}. The instance must be wired prior to it's
	 * usage to ensure that a {@code baseIndexFactory} is available.
	 */
	public MetaDataModel() {
		this(null);
	}

	/**
	 * Creates a {@code MetaDataModel} with the specified
	 * {@code baseIndexFactory}, which should not be {@code null}. If the
	 * {@code baseIndexFactory} should be {@code null} use another constructor
	 * and read it's information.
	 * 
	 * @param baseIndexFactory
	 *            the {@code baseIndexFactory} used to determine the indexes to
	 *            be used
	 * 
	 * @see BaseIndexFactory
	 */
	public MetaDataModel(final BaseIndexFactory baseIndexFactory) {

		// set the factories
		this.indexFactory = baseIndexFactory;
		setOfflineMode(null);
	}

	/**
	 * Adds the meta-data defined by {@code MetaDataCollection} to the
	 * {@code MetaDataModel}.
	 * 
	 * @param metaData
	 *            the meta-data to be added
	 * 
	 * @see IMetaDataCollection
	 */
	@SuppressWarnings("unchecked")
	public void addMetaData(final IMetaDataCollection metaData) {

		// iterate over the metadata and add it
		for (final IMetaData md : metaData) {
			final String modelId = md.getDescriptorModelId();

			@SuppressWarnings("rawtypes")
			final DescriptorModel model = getDescriptorModel(modelId);

			// check if the metaData provides identifier
			if (md instanceof IIdentifiedMetaData) {
				final IIdentifiedMetaData idMetaData = (IIdentifiedMetaData) md;

				for (final Entry<Object, Object> entry : idMetaData
						.getIdentifiedValues().entrySet()) {
					final Object key = entry.getKey();
					final Object value = entry.getValue();

					model.addDescriptor(model.castId(key), value);
				}
			} else {

				// set the offline mode if it's aware of one
				if (md instanceof IOfflineModeAwareMetaData) {
					((IOfflineModeAwareMetaData) md)
							.setOfflineMode(getOfflineMode());
				}

				// create a descriptor for each value, if it doesn't exist
				for (final Object value : md.getValues()) {
					if (model.getDescriptorByValue(value) == null) {
						model.createDescriptor(value);
					} else {
						if (LOG.isTraceEnabled()) {
							LOG.trace("Skipping to add '"
									+ value
									+ "' because a descriptor is already available.");
						}
					}
				}
			}
		}
	}

	/**
	 * Creates a {@code Descriptor} for the specified {@code DescriptorModel}.
	 * 
	 * @param modelId
	 *            the id of the {@code DescriptorModel} to create the
	 *            {@code Descriptor} for
	 * @param value
	 *            the value of the {@code Descriptor} to be added
	 * 
	 * @return the created {@code Descriptor}
	 */
	public <D> Descriptor<D, ?, ?> createDescriptor(final String modelId,
			final D value) {
		final DescriptorModel<?> model = getDescriptorModel(modelId);
		if (model == null) {
			exceptionRegistry.throwException(MetaDataModelException.class,
					1002, modelId);
		}

		return model.createDescriptor(value);
	}

	/**
	 * Gets all the {@code DescriptorModel} instances of {@code this} model.
	 * 
	 * @return all the {@code DescriptorModel} instances of {@code this} model
	 */
	public Collection<DescriptorModel<?>> getDescriptorModels() {

		@SuppressWarnings("unchecked")
		final Collection<DescriptorModel<?>> coll = (Collection<DescriptorModel<?>>) getDescriptorModelsIndex()
				.getAll();

		return coll;
	}

	/**
	 * Get the {@code DescriptorModel} with the specified {@code id} of
	 * {@code this} model.
	 * 
	 * @param id
	 *            the id of the {@code DescriptorModel} to be retrieved
	 * @return the {@code DescriptorModel} with the specified {@code id} of
	 *         {@code this} model or {@code null} if no such
	 *         {@code DescriptorModel} exists
	 */
	public DescriptorModel<?> getDescriptorModel(final String id) {
		if (id == null) {
			return null;
		} else {
			return (DescriptorModel<?>) getDescriptorModelsIndex()
					.getObject(id);
		}
	}

	/**
	 * Adds all the {@code descriptorModels} to {@code this} model.
	 * 
	 * @param descriptorModels
	 *            the {@code descriptorModels} to be added
	 */
	protected void addDescriptorModels(
			final Collection<DescriptorModel<?>> descriptorModels) {
		if (descriptorModels != null) {
			for (final DescriptorModel<?> descriptorModel : descriptorModels) {
				addDescriptorModel(descriptorModel);
			}
		}
	}

	/**
	 * Add the {@code descriptorModel} to {@code this} model.
	 * 
	 * @param descriptorModel
	 *            the {@code descriptorModel} to {@code this} model.
	 */
	protected void addDescriptorModel(final DescriptorModel<?> descriptorModel) {
		if (descriptorModel == null) {
			exceptionRegistry
					.throwException(MetaDataModelException.class, 1007);
		} else if (getDescriptorModelsIndex()
				.getObject(descriptorModel.getId()) != null) {
			exceptionRegistry.throwException(MetaDataModelException.class,
					1005, descriptorModel.getId());
		} else {
			descriptorModel.setOfflineMode(getOfflineMode());
			getDescriptorModelsIndex().addObject(descriptorModel);
		}
	}

	/**
	 * Gets the {@code Descriptor} of a specific model with the specified
	 * {@code id}.
	 * 
	 * @param modelId
	 *            the identifier of the {@code DescriptorModel} to look for the
	 *            value
	 * @param id
	 *            the id of the {@code Descriptor} to be retrieved
	 * 
	 * @return the {@code Descriptor} found, or {@code null} if no descriptor
	 *         was found
	 * 
	 * @throws MetaDataModelException
	 *             if the specified model is not part of {@code this}
	 *             {@code MetaDataModel}
	 */
	@SuppressWarnings("unchecked")
	public <I> Descriptor<?, ?, I> getDescriptor(final String modelId,
			final I id) {
		final DescriptorModel<I> model = (DescriptorModel<I>) getDescriptorModel(modelId);
		if (model == null) {
			exceptionRegistry.throwException(MetaDataModelException.class,
					1004, modelId);
		}

		return (Descriptor<?, ?, I>) model.getDescriptor(id);
	}

	/**
	 * Gets the {@code Descriptor} of a specific model with the specified
	 * {@code value}.
	 * 
	 * @param modelId
	 *            the identifier of the {@code DescriptorModel} to look for the
	 *            value
	 * @param value
	 *            the value of the {@code Descriptor} to be retrieved
	 * 
	 * @return the {@code Descriptor} found, or {@code null} if no descriptor
	 *         was found
	 * 
	 * @throws MetaDataModelException
	 *             if the specified model is not part of {@code this}
	 *             {@code MetaDataModel}
	 */
	@SuppressWarnings("unchecked")
	public <D> Descriptor<D, ?, ?> getDescriptorByValue(final String modelId,
			final D value) throws MetaDataModelException {
		final DescriptorModel<?> model = getDescriptorModel(modelId);
		if (model == null) {
			exceptionRegistry.throwException(MetaDataModelException.class,
					1004, modelId);
		}

		return (Descriptor<D, ?, ?>) model.getDescriptorByValue(value);
	}

	/**
	 * Gets the descriptor for the specified value of the specified
	 * {@code DescriptorModel}. If the descriptor is not found, a strategy based
	 * on the passed {@code MetaDataHandling} is applied.
	 * 
	 * @param modelId
	 *            the identifier of the {@code DescriptorModel} to look up the
	 *            value
	 * @param value
	 *            the value of the {@code Descriptor} to be returned
	 * @param handling
	 *            the handling strategy
	 * 
	 * @return the {@code Descriptor} with the specified {@code value}
	 * @throws MetaDataModelException
	 * 
	 * @see MetaDataHandling
	 */
	@SuppressWarnings("unchecked")
	public <D> Descriptor<D, ?, ?> getDescriptorByValue(final String modelId,
			final D value, final MetaDataHandling handling)
			throws MetaDataModelException {
		final DescriptorModel<?> model = getDescriptorModel(modelId);
		if (model == null) {
			exceptionRegistry.throwException(MetaDataModelException.class,
					1004, modelId);
		}

		return (Descriptor<D, ?, ?>) model
				.getDescriptorByValue(value, handling);
	}

	/**
	 * Get all the {@code Descriptor} instances stored within the
	 * {@code MetaDataModel}.
	 * 
	 * @return all the {@code Descriptor} instances stored within the
	 *         {@code MetaDataModel}
	 */
	public Collection<Descriptor<?, ?, ?>> getDescriptors() {
		final Collection<DescriptorModel<?>> descriptorModels = getDescriptorModels();

		final List<Descriptor<?, ?, ?>> descriptors = new ArrayList<Descriptor<?, ?, ?>>();
		for (final DescriptorModel<?> descriptorModel : descriptorModels) {
			descriptors.addAll(descriptorModel.getDescriptors());
		}

		return descriptors;
	}

	/**
	 * Get all the {@code Descriptor} instances stored within the
	 * {@code MetaDataModel} of a specific {@code descriptorClazz}.
	 * 
	 * @param descriptorClazz
	 *            the class of which the {@code Descriptors} should be
	 * 
	 * @return all the {@code Descriptor} instances stored within the
	 *         {@code MetaDataModel} of a specific {@code descriptorClazz}
	 */
	public Collection<Descriptor<?, ?, ?>> getDescriptorsByClass(
			final Class<?>... descriptorClazz) {
		final Collection<DescriptorModel<?>> descriptorModels = getDescriptorModels();

		final List<Descriptor<?, ?, ?>> descriptors = new ArrayList<Descriptor<?, ?, ?>>();
		if (descriptorClazz == null) {
			return descriptors;
		}

		// check the data of each DescriptorModel
		for (final DescriptorModel<?> descriptorModel : descriptorModels) {

			@SuppressWarnings("unchecked")
			final Class<? extends Descriptor<?, ?, ?>> mdc = (Class<? extends Descriptor<?, ?, ?>>) descriptorModel
					.getDescriptorClass();

			// make sure it's the correct type
			boolean add = false;
			for (Class<?> dc : descriptorClazz) {
				if (dc.isAssignableFrom(mdc)) {
					add = true;
					break;
				}
			}

			if (add) {
				descriptors.addAll(descriptorModel.getDescriptors());
			}
		}

		return descriptors;
	}

	/**
	 * Gets the {@code baseIndexFactory} specified for the {@code MetaDataModel}
	 * . This method should never return {@code null} if the
	 * {@code MetaDataModel} is assumed to be initialized.
	 * 
	 * @return the {@code baseIndexFactory} specified for the
	 *         {@code MetaDataModel}
	 */
	public BaseIndexFactory getIndexFactory() {
		return indexFactory;
	}

	/**
	 * Gets the {@code Index} used to index the different
	 * {@code DescriptorModel} instances. This method never returns {@code null}
	 * , but throws an exception if the {@code baseIndexFactory} isn't defined
	 * by wiring or construction.
	 * 
	 * @return the {@code Index} used to index the different
	 *         {@code DescriptorModel} instances
	 * 
	 * @throws RuntimeException
	 *             can throw any exception if the {@code baseIndexFactory} is
	 *             {@code null}
	 */
	public IIndexedCollection getDescriptorModelsIndex() {
		if (descriptorModels == null) {
			final IndexKeyDefinition key = new IndexKeyDefinition(
					DescriptorModel.class, "getId");
			descriptorModels = getIndexFactory().create(key);
		}

		return descriptorModels;
	}

	/**
	 * Gets the current setting of the {@code OfflineMode}.
	 * 
	 * @return the settings of the {@code OfflineMode}
	 * 
	 * @see OfflineMode
	 */
	public OfflineMode getOfflineMode() {
		return this.offlineMode;
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

		// nothing indexed so far, because no factory defined
		if (getIndexFactory() == null) {
			return;
		}

		// change the mode for all the models created so far
		for (final DescriptorModel<?> model : getDescriptorModels()) {
			model.setOfflineMode(this.offlineMode);
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

	@Override
	public String toString() {
		return descriptorModels.toString();
	}
}
