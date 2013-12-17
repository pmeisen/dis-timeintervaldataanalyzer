package net.meisen.dissertation.models.impl.data;

import net.meisen.general.genmisc.types.Objects;

/**
 * A descriptor is used to describe {@code Resource} instances within an
 * {@code RawTimeInterval}.
 * 
 * @author pmeisen
 * 
 * @param <D>
 *            the type of the value of the descriptor
 * @param <T>
 *            the concrete {@code Descriptor} class
 * @param <I>
 *            the type of the identifier
 */
public abstract class Descriptor<D extends Object, T extends Descriptor<D, T, I>, I extends Object> {
	private final I id;
	private final DescriptorModel model;

	/**
	 * Constructor which creates a {@code Descriptor} based on the specified
	 * {@code model} and with the specified {@code id}.
	 * 
	 * @param model
	 *            the {@code DescriptorModel} of the descriptor
	 * @param id
	 *            the identifier of the descriptor
	 */
	public Descriptor(final DescriptorModel model, final I id) {
		this.id = id;
		this.model = model;
	}

	/**
	 * Gets the identifier of the descriptor.
	 * 
	 * @return the identifier of the descriptor
	 */
	public I getId() {
		return id;
	}

	/**
	 * Gets the {@code DescriptorModel} of this descriptor
	 * 
	 * @return the {@code DescriptorModel} of this descriptor
	 */
	protected DescriptorModel getModel() {
		return model;
	}

	/**
	 * The name of the model used by this descriptor
	 * 
	 * @return the name of the model
	 */
	public String getModelName() {
		return model.getName();
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
	 * Gets the value of the {@code DoubleDescriptor}.
	 * 
	 * @return the value of the {@code DoubleDescriptor}
	 */
	public abstract D getValue();

	@Override
	public abstract T clone();

	@Override
	public abstract String toString();

	@Override
	public boolean equals(final Object o) {
		boolean cmp = false;

		if (o == this) {
			cmp = true;
		} else if (o == null) {
			// nothing to do
		} else if (getClass().equals(o.getClass())) {

			@SuppressWarnings("unchecked")
			final Descriptor<D, ?, ?> d = (Descriptor<D, ?, ?>) o;

			if (Objects.equals(getValue(), d.getValue())
					&& Objects.equals(getModel(), d.getModel())) {
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
