package net.meisen.dissertation.models.impl.data;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.UUID;

import net.meisen.dissertation.config.xslt.DefaultValues;
import net.meisen.dissertation.exceptions.MetaDataModelException;
import net.meisen.dissertation.models.IIndexedCollection;
import net.meisen.dissertation.models.impl.indexes.IndexKeyDefinition;
import net.meisen.dissertation.models.impl.indexes.BaseIndexedCollectionFactory;
import net.meisen.general.genmisc.exceptions.registry.IExceptionRegistry;

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

	@Autowired
	@Qualifier(DefaultValues.EXCEPTIONREGISTRY_ID)
	private IExceptionRegistry exceptionRegistry;

	@Autowired
	@Qualifier(DefaultValues.INDEXFACTORY_ID)
	private BaseIndexedCollectionFactory baseIndexedCollectionFactory;

	private final String id;
	private final String name;

	private IIndexedCollection descriptorModels;

	/**
	 * Creates a {@code MetaDataModel} with a random id, the instance must be
	 * wired prior to it's usage to ensure that a
	 * {@code baseIndexedCollectionFactory} is available.
	 */
	public MetaDataModel() {
		this(null, null, null);
	}

	/**
	 * Creates a {@code MetaDataModel} with the specified {@code id}, the
	 * instance must be wired prior to it's usage to ensure that a
	 * {@code baseIndexedCollectionFactory} is available.
	 * 
	 * @param id
	 *            the identifier used for the {@code MetaDataModel}
	 */
	public MetaDataModel(final String id) {
		this(id, null, null);
	}

	/**
	 * Creates a {@code MetaDataModel} with the specified {@code id} and the
	 * specified {@code name}, the instance must be wired prior to it's usage to
	 * ensure that a {@code baseIndexedCollectionFactory} is available.
	 * 
	 * @param id
	 *            the identifier used for the {@code MetaDataModel}
	 * @param name
	 *            the name of the {@code MetaDataModel}, if {@code null} the
	 *            name will be equal to the {@code id}
	 */
	public MetaDataModel(final String id, final String name) {
		this(id, name, null);
	}

	/**
	 * Creates a {@code MetaDataModel} with a random id and the specified
	 * {@code baseIndexedCollectionFactory}, which should not be {@code null}. If
	 * the {@code baseIndexedCollectionFactory} should be {@code null} use another
	 * constructor and read it's information.
	 * 
	 * @param baseIndexedCollectionFactory
	 *            the {@code BaseIndexedCollectionFactory} used to determine the
	 *            indexes to be used
	 * 
	 * @see BaseIndexedCollectionFactory
	 */
	public MetaDataModel(final BaseIndexedCollectionFactory baseIndexedCollectionFactory) {
		this(null, null, baseIndexedCollectionFactory);
	}

	/**
	 * Creates a {@code MetaDataModel} with the specified {@code id}, the
	 * specified {@code name} and the specified {@code baseIndexedCollectionFactory}
	 * , which should not be {@code null}. If the
	 * {@code baseIndexedCollectionFactory} should be {@code null} use another
	 * constructor and read it's information.
	 * 
	 * @param id
	 *            the identifier used for the {@code MetaDataModel}
	 * @param baseIndexedCollectionFactory
	 *            the {@code BaseIndexedCollectionFactory} used to determine the
	 *            indexes to be used
	 * 
	 * @see BaseIndexedCollectionFactory
	 */
	public MetaDataModel(final String id,
			final BaseIndexedCollectionFactory baseIndexedCollectionFactory) {
		this(id, null, baseIndexedCollectionFactory);
	}

	/**
	 * Creates a {@code MetaDataModel} with the specified {@code id} and the
	 * specified {@code baseIndexedCollectionFactory}, which should not be
	 * {@code null}. If the {@code baseIndexedCollectionFactory} should be
	 * {@code null} use another constructor and read it's information.
	 * 
	 * @param id
	 *            the identifier used for the {@code MetaDataModel}
	 * @param name
	 *            the name of the {@code MetaDataModel}, if {@code null} the
	 *            name will be equal to the {@code id}
	 * @param baseIndexedCollectionFactory
	 *            the {@code BaseIndexedCollectionFactory} used to determine the
	 *            indexes to be used
	 * 
	 * @see BaseIndexedCollectionFactory
	 */
	public MetaDataModel(final String id, final String name,
			final BaseIndexedCollectionFactory baseIndexedCollectionFactory) {

		// set id and name
		this.id = id == null ? UUID.randomUUID().toString() : id;
		this.name = name == null ? id : name;

		// set the factories
		this.baseIndexedCollectionFactory = baseIndexedCollectionFactory;
	}

	/**
	 * Gets the identifier of the {@code MetaDataModel}.
	 * 
	 * @return the identifier of the {@code MetaDataModel}
	 */
	public String getId() {
		return id;
	}

	/**
	 * Gets the name of the {@code MetaDataModel}.
	 * 
	 * @return the name of the {@code MetaDataModel}
	 */
	public String getName() {
		return name;
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
	public <D, T extends Descriptor<D, T, ?>> Descriptor<D, T, ?> createDescriptor(
			final String modelId, final D value) {
		final DescriptorModel<?> model = getDescriptorModel(modelId);
		if (model == null) {
			exceptionRegistry.throwException(MetaDataModelException.class,
					1002, modelId);
		}

		@SuppressWarnings("unchecked")
		final Descriptor<D, T, ?> descriptor = (Descriptor<D, T, ?>) model
				.createDescriptor(value);
		return descriptor;
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
					1002, modelId);
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
					1002, modelId);
		}

		return (Descriptor<D, ?, ?>) model.getDescriptorByValue(value);
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
	 * Gets the {@code baseIndexedCollectionFactory} specified for the
	 * {@code MetaDataModel}. This method should never return {@code null} if
	 * the {@code MetaDataModel} is assumed to be initialized.
	 * 
	 * @return the {@code baseIndexedCollectionFactory} specified for the
	 *         {@code MetaDataModel}
	 */
	public BaseIndexedCollectionFactory getIndexedCollectionFactory() {
		return baseIndexedCollectionFactory;
	}

	/**
	 * Gets the {@code Index} used to index the different
	 * {@code DescriptorModel} instances. This method never returns {@code null}
	 * , but throws an exception if the {@code baseIndexedCollectionFactory} isn't
	 * defined by wiring or construction.
	 * 
	 * @return the {@code Index} used to index the different
	 *         {@code DescriptorModel} instances
	 * 
	 * @throws RuntimeException
	 *             can throw any exception if the
	 *             {@code baseIndexedCollectionFactory} is {@code null}
	 */
	public IIndexedCollection getDescriptorModelsIndex() {
		if (descriptorModels == null) {
			final IndexKeyDefinition key = new IndexKeyDefinition(
					DescriptorModel.class, "getId");
			descriptorModels = getIndexedCollectionFactory().create(key);
		}

		return descriptorModels;
	}
}
