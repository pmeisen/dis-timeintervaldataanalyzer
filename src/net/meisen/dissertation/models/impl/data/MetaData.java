package net.meisen.dissertation.models.impl.data;

import java.util.Collection;

/**
 * A {@code MetaData} instance defines some data which is used by the
 * {@code MetaDataModel}.
 * 
 * @author pmeisen
 * 
 */
public class MetaData {
	private final Collection<ResourceModel> resourceModels;
	private final Collection<DescriptorModel> descriptorModels;
	private final Collection<Resource<?>> resources;
	private final Collection<Descriptor<?, ?, ?>> descriptors;

	/**
	 * Constructor to define the {@code MetaData}.
	 * 
	 * @param resourceModels
	 *            the {@code ResourceModel} instances of this {@code MetaData}
	 * @param descriptorModels
	 *            the {@code DescriptorModel} instances of this {@code MetaData}
	 * @param resources
	 *            the {@code Resource} instances of this {@code MetaData}
	 * @param descriptors
	 *            the {@code Descriptor} instances of this {@code MetaData}
	 */
	public MetaData(final Collection<ResourceModel> resourceModels,
			final Collection<DescriptorModel> descriptorModels,
			final Collection<Resource<?>> resources,
			final Collection<Descriptor<?, ?, ?>> descriptors) {
		this.resourceModels = resourceModels;
		this.descriptorModels = descriptorModels;
		this.resources = resources;
		this.descriptors = descriptors;
	}

	/**
	 * Get the {@code ResourceModel} instances of this {@code MetaData}.
	 * 
	 * @return the {@code ResourceModel} instances of this {@code MetaData}
	 */
	public Collection<ResourceModel> getResourceModels() {
		return resourceModels;
	}

	/**
	 * Get the {@code DescriptorModel} instances of this {@code MetaData}.
	 * 
	 * @return the {@code DescriptorModel} instances of this {@code MetaData}
	 */
	public Collection<DescriptorModel> getDescriptorModels() {
		return descriptorModels;
	}

	/**
	 * Get the {@code Resource} instances of this {@code MetaData}.
	 * 
	 * @return the {@code Resource} instances of this {@code MetaData}
	 */
	public Collection<Resource<?>> getResources() {
		return resources;
	}

	/**
	 * Get the {@code Descriptor} instances of this {@code MetaData}.
	 * 
	 * @return the {@code Descriptor} instances of this {@code MetaData}
	 */
	public Collection<Descriptor<?, ?, ?>> getDescriptors() {
		return descriptors;
	}
}
