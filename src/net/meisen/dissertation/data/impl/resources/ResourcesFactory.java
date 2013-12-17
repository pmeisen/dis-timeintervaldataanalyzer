package net.meisen.dissertation.data.impl.resources;

import net.meisen.dissertation.data.IIdsFactory;
import net.meisen.dissertation.models.impl.data.Resource;
import net.meisen.dissertation.models.impl.data.ResourceModel;

/**
 * Factory to create a {@code Resource} based on the {@code ResourceModel} and a
 * value string.
 * 
 * @author pmeisen
 * 
 */
public class ResourcesFactory {

	private final IIdsFactory<?> idsFactory;

	/**
	 * Constructor which specifies the {@code IdsFactory} to be used.
	 * 
	 * @param idsFactory
	 *            the {@code IdsFactory} to be used
	 * 
	 * @see IIdsFactory
	 */
	public ResourcesFactory(final IIdsFactory<?> idsFactory) {
		this.idsFactory = idsFactory;
	}

	/**
	 * Creates a {@code Resource} based on the specified {@code model} and the
	 * {@code value}.
	 * 
	 * @param model
	 *            the {@code ResourceModel} of the {@code Resource} to be
	 *            created
	 * @param value
	 *            the {@code value} of the {@code Resource} to be created
	 * 
	 * @return the created {@code Resource} instance
	 */
	public Resource<?> createResource(final ResourceModel model,
			final String value) {
		return new Resource<Object>(model, idsFactory.getId(), value);
	}

	/**
	 * Gets the type of the identifier.
	 * 
	 * @return the type of the identifier
	 */
	public Class<?> getIdClass() {
		return idsFactory.getIdClass();
	}
}
