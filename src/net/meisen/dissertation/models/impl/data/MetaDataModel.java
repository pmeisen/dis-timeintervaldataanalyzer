package net.meisen.dissertation.models.impl.data;

import java.util.Collection;
import java.util.UUID;

import net.meisen.dissertation.config.xslt.DefaultValues;
import net.meisen.dissertation.data.impl.descriptors.DescriptorsFactory;
import net.meisen.dissertation.data.impl.indexes.IndexedCollectionFactory;
import net.meisen.dissertation.data.impl.resources.ResourcesFactory;
import net.meisen.dissertation.exceptions.MetaDataModelException;
import net.meisen.dissertation.models.IIndexedCollection;
import net.meisen.dissertation.models.IMultipleKeySupport;
import net.meisen.dissertation.models.impl.indexes.IndexKeyDefinition;
import net.meisen.general.genmisc.exceptions.registry.IExceptionRegistry;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;

public class MetaDataModel {

	@Autowired
	@Qualifier(DefaultValues.EXCEPTIONREGISTRY_ID)
	private IExceptionRegistry exceptionRegistry;

	@Autowired(required = false)
	private Collection<MetaData> metaData;

	@Autowired
	@Qualifier(DefaultValues.RESOURCESFACTORY_ID)
	private final ResourcesFactory resourcesFactory;

	@Autowired
	@Qualifier(DefaultValues.DESCRIPTORSFACTORY_ID)
	private final DescriptorsFactory descriptorsFactory;

	@Autowired
	@Qualifier(DefaultValues.INDEXFACTORY_ID)
	private final IndexedCollectionFactory indexedCollectionFactory;

	private final String id;
	private final String name;

	private IMultipleKeySupport resources;
	private IMultipleKeySupport descriptors;

	private IIndexedCollection resourceModels;
	private IIndexedCollection descriptorModels;

	public MetaDataModel() {
		this(null, null, null, null, null);
	}

	public MetaDataModel(final String id) {
		this(id, null, null, null, null);
	}

	public MetaDataModel(final String id, final String name) {
		this(id, name, null, null, null);
	}

	public MetaDataModel(final ResourcesFactory resourceFactory,
			final DescriptorsFactory descriptorFactory,
			final IndexedCollectionFactory indexedCollectionFactory) {
		this(null, null, resourceFactory, descriptorFactory,
				indexedCollectionFactory);
	}

	public MetaDataModel(final String id,
			final ResourcesFactory resourceFactory,
			final DescriptorsFactory descriptorFactory,
			final IndexedCollectionFactory indexedCollectionFactory) {
		this(id, null, resourceFactory, descriptorFactory,
				indexedCollectionFactory);
	}

	public MetaDataModel(final String id, final String name,
			final ResourcesFactory resourceFactory,
			final DescriptorsFactory descriptorFactory,
			final IndexedCollectionFactory indexedCollectionFactory) {

		// set id and name
		this.id = id == null ? UUID.randomUUID().toString() : id;
		this.name = name == null ? id : name;

		// set the factories
		this.resourcesFactory = resourceFactory;
		this.descriptorsFactory = descriptorFactory;
		this.indexedCollectionFactory = indexedCollectionFactory;
	}

	public String getId() {
		return id;
	}

	public String getName() {
		return name;
	}

	/**
	 * Initializes any auto-wired {@code MetaData} and validates other
	 * auto-wired fields.
	 */
	public void init() {

		if (exceptionRegistry == null) {
			throw new NullPointerException(
					"The exceptionRegistry of a MetaDataModel cannot be null!");
		} else if (indexedCollectionFactory == null) {
			throw new NullPointerException(
					"The indexedCollectionFactory of a MetaDataModel cannot be null!");
		} else if (resourcesFactory == null) {
			throw new NullPointerException(
					"The resourcesFactory of a MetaDataModel cannot be null!");
		} else if (descriptorsFactory == null) {
			throw new NullPointerException(
					"The descriptorsFactory of a MetaDataModel cannot be null!");
		}

		// get the indexes, use the factory to decide which one is best
		this.resourceModels = indexedCollectionFactory
				.create(new IndexKeyDefinition(ResourceModel.class, "getId"));
		this.descriptorModels = indexedCollectionFactory
				.create(new IndexKeyDefinition(DescriptorModel.class, "getId"));

		final IndexKeyDefinition resIdDef = new IndexKeyDefinition(
				Resource.class, "getId");
		final IndexKeyDefinition uniqueResDef = new IndexKeyDefinition(
				Resource.class, "getModelId", "getValue");
		resIdDef.overrideType(0, this.resourcesFactory.getIdClass());
		this.resources = indexedCollectionFactory
				.create(resIdDef, uniqueResDef);

		final IndexKeyDefinition desIdDef = new IndexKeyDefinition(
				Descriptor.class, "getId");
		final IndexKeyDefinition uniqueDesDef = new IndexKeyDefinition(
				Descriptor.class, "getModelId", "getValue");
		desIdDef.overrideType(0, this.descriptorsFactory.getIdClass());
		this.descriptors = indexedCollectionFactory.create(desIdDef,
				uniqueDesDef);

		// add all the data from the defined metaData
		if (metaData != null && metaData.size() > 0) {
			for (final MetaData md : metaData) {
				addResourceModels(md.getResourceModels());
				addDescriptorModels(md.getDescriptorModels());
				addResources(md.getResources());
				addDescriptors(md.getDescriptors());
			}
		}
	}

	protected ResourceModel getResourceModel(final String id) {
		return (ResourceModel) resourceModels.getObject(id);
	}

	protected DescriptorModel getDescriptorModel(final String id) {
		return (DescriptorModel) descriptorModels.getObject(id);
	}

	protected void addResourceModels(
			final Collection<ResourceModel> resourceModels) {
		if (resourceModels != null) {
			for (final ResourceModel resourceModel : resourceModels) {
				addResourceModel(resourceModel);
			}
		}
	}

	protected void addResourceModel(final ResourceModel resourceModel) {
		if (resourceModel == null) {
			exceptionRegistry
					.throwException(MetaDataModelException.class, 1006);
		} else if (resourceModels.getObject(resourceModel.getId()) != null) {
			exceptionRegistry.throwException(MetaDataModelException.class,
					1004, resourceModel.getId());
		} else {
			resourceModels.addObject(resourceModel);
		}

	}

	protected void addDescriptorModels(
			final Collection<DescriptorModel> descriptorModels) {
		if (descriptorModels != null) {
			for (final DescriptorModel descriptorModel : descriptorModels) {
				addDescriptorModel(descriptorModel);
			}
		}
	}

	protected void addDescriptorModel(final DescriptorModel descriptorModel) {
		if (descriptorModel == null) {
			exceptionRegistry
					.throwException(MetaDataModelException.class, 1007);
		} else if (descriptorModels.getObject(descriptorModel.getId()) != null) {
			exceptionRegistry.throwException(MetaDataModelException.class,
					1005, descriptorModel.getId());
		} else {
			descriptorModels.addObject(descriptorModel);
		}
	}

	public Resource<?> createResource(final String modelId, final String value) {
		final ResourceModel model = getResourceModel(modelId);
		if (model == null) {
			exceptionRegistry.throwException(MetaDataModelException.class,
					1000, modelId);
		}

		return createResource(model, value);
	}

	public Resource<?> createResource(final ResourceModel model,
			final String value) {
		if (model == null) {
			exceptionRegistry
					.throwException(MetaDataModelException.class, 1001);
		}

		// create the resource and add it
		final Resource<?> resource = resourcesFactory.createResource(model,
				value);
		this.addResource(resource);

		return resource;
	}

	public Descriptor<?, ?, ?> createDescriptor(final String modelId,
			final Object value) {
		final DescriptorModel model = getDescriptorModel(modelId);
		if (model == null) {
			exceptionRegistry.throwException(MetaDataModelException.class,
					1002, modelId);
		}

		return createDescriptor(model, value);
	}

	public Descriptor<?, ?, ?> createDescriptor(final DescriptorModel model,
			final Object value) {
		if (model == null) {
			exceptionRegistry
					.throwException(MetaDataModelException.class, 1003);
		}

		// create the descriptor and add it
		final Descriptor<?, ?, ?> descriptor = descriptorsFactory
				.createDescriptor(model, value);
		this.addDescriptor(descriptor);

		return descriptor;
	}

	protected void addResources(final Collection<Resource<?>> resources) {
		if (resources != null) {
			for (final Resource<?> resource : resources) {
				addResource(resource);
			}
		}
	}

	protected void addResource(final Resource<?> resource) {
		if (!resources.addObject(resource)) {
			exceptionRegistry.throwException(MetaDataModelException.class,
					1008, resource, resource.getModelId());
		}
	}

	protected void addDescriptors(
			final Collection<Descriptor<?, ?, ?>> descriptors) {

		if (descriptors != null) {
			for (final Descriptor<?, ?, ?> descriptor : descriptors) {
				addDescriptor(descriptor);
			}
		}
	}

	protected void addDescriptor(final Descriptor<?, ?, ?> descriptor) {
		if (!descriptors.addObject(descriptor)) {
			exceptionRegistry.throwException(MetaDataModelException.class,
					1009, descriptor, descriptor.getModelId());
		}
	}

	@SuppressWarnings("unchecked")
	public <D> Descriptor<D, ?, ?> getDescriptor(final String modelId,
			final D value) {
		return (Descriptor<D, ?, ?>) descriptors.getObject(modelId, value);
	}

	@SuppressWarnings("unchecked")
	public <I> Descriptor<?, ?, I> getDescriptor(final I id) {
		return (Descriptor<?, ?, I>) descriptors.getObject(id);
	}

	public Resource<?> getResource(final String modelId, final String value) {
		return (Resource<?>) resources.getObject(modelId, value);
	}

	@SuppressWarnings("unchecked")
	public <I> Resource<I> getResource(final I id) {
		return (Resource<I>) resources.getObject(id);
	}

	@SuppressWarnings("unchecked")
	public Collection<Resource<?>> getResources() {
		return (Collection<Resource<?>>) resources.getAll();
	}

	@SuppressWarnings("unchecked")
	public Collection<Descriptor<?, ?, ?>> getDescriptors() {
		return (Collection<Descriptor<?, ?, ?>>) descriptors.getAll();
	}
}
