package net.meisen.dissertation.models.impl.data;

import net.meisen.general.genmisc.types.Objects;

/**
 * A {@code Resource} is something which is consumed by an interval, i.e. it
 * identifies that this interval blocks the resource.
 * 
 * @author pmeisen
 * 
 * @param <I>
 *            the type of the identifier used by the resource
 */
public class Resource<I extends Object> {
	private final I id;
	private final ResourceModel model;
	private final String value;

	/**
	 * Constructor used to create a {@code Resource}.
	 * 
	 * @param model
	 *            the {@code ResourceModel} which defines this {@code Resource}
	 * @param id
	 *            the identifier of the {@code Resource}
	 * @param value
	 *            the value of the {@code Resource}
	 */
	public Resource(final ResourceModel model, final I id, final String value) {
		this.model = model;
		this.id = id;
		this.value = value;
	}

	/**
	 * Gets the value of the {@code Resource}
	 * 
	 * @return the value of the {@code Resource}
	 */
	public String getValue() {
		return value;
	}

	/**
	 * Gets the {@code DescriptorModel} of this descriptor
	 * 
	 * @return the {@code DescriptorModel} of this descriptor
	 */
	public String getModelName() {
		return model.getName();
	}

	/**
	 * Gets the identifier of the {@code Resource}.
	 * 
	 * @return the identifier of the {@code Resource}
	 */
	public I getId() {
		return id;
	}

	/**
	 * Gets the identifier of the {@code ResourceModel}.
	 * 
	 * @return the identifier of the {@code ResourceModel}
	 */
	public String getModelId() {
		return getModel().getId();
	}

	/**
	 * Gets the {@code ResourceModel} of this {@code Resource}.
	 * 
	 * @return the {@code ResourceModel} of this {@code Resource}
	 */
	protected ResourceModel getModel() {
		return model;
	}

	@Override
	public Resource<I> clone() {
		final Resource<I> clone = new Resource<I>(getModel(), getId(),
				getValue());

		return clone;
	}

	@Override
	public String toString() {
		return value;
	}

	@Override
	public boolean equals(final Object o) {
		boolean cmp = false;

		if (o == this) {
			cmp = true;
		} else if (o == null) {
			// nothing to do
		} else if (getClass().equals(o.getClass())) {
			final Resource<?> r = (Resource<?>) o;

			if (getId().equals(r.getId())
					&& Objects.equals(getModel(), r.getModel())) {
				cmp = true;
			}
		}

		return cmp;
	}

	@Override
	public int hashCode() {
		return id.hashCode();
	}
}
