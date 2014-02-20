package net.meisen.dissertation.model.descriptors;

import net.meisen.general.genmisc.types.Objects;

/**
 * A descriptor is used to describe meta information of data. A
 * {@code Descriptor} should always have a unique string representation, which
 * is returned when using the {@link #getValueStringRepresentative()} method. A
 * concrete implementation should always be able to handle the string
 * representative and therefore have a valid constructor.
 * 
 * <pre>
 * public ConcreteDescriptor(DescriptorModel&lt;I&gt; model, I id, String value) {
 * 	super(model, id);
 * 	this.value = resolveFromStringRepresentation(value);
 * }
 * </pre>
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
	private final DescriptorModel<I> model;

	/**
	 * Constructor which creates a {@code Descriptor} based on the specified
	 * {@code model} and with the specified {@code id}.
	 * 
	 * @param model
	 *            the {@code DescriptorModel} of the descriptor
	 * @param id
	 *            the identifier of the descriptor
	 */
	public Descriptor(final DescriptorModel<I> model, final I id) {
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
	protected DescriptorModel<I> getModel() {
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
	 * Gets the class of the identifier used by this {@code Descriptor}. This
	 * class is equal to the generic-parameter {@code I}.
	 * 
	 * @return the class of the identifier used by this {@code Descriptor}
	 */
	public Class<?> getIdClass() {
		return getModel().getIdClass();
	}

	/**
	 * Gets the value of the {@code DoubleDescriptor}.
	 * 
	 * @return the value of the {@code DoubleDescriptor}
	 */
	public abstract D getValue();

	@Override
	public abstract T clone();

	/**
	 * The string representation of a {@code Descriptor} should be always
	 * resolvable to a {@code Descriptor} if it's used as value. The
	 * {@link DescriptorModel} tries to guess the constructor available for a
	 * value-type (i.e. a string). Therefore every Descriptor should have a
	 * constructor:
	 * 
	 * <pre>
	 * public ConcreteDescriptor(DescriptorModel&lt;I&gt; model, I id, String value) {
	 * 	super(model, id);
	 * 	this.value = resolveFromStringRepresentation(value);
	 * }
	 * </pre>
	 */
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

	/**
	 * A string representation of the {@code Descriptor} which is unique for the
	 * value. This method is quiet important for storing, resolving and mapping
	 * the descriptor.
	 * 
	 * <pre>
	 * public ConcreteDescriptor(DescriptorModel&lt;I&gt; model, I id, String value) {
	 * 	super(model, id);
	 * 	this.value = resolveFromStringRepresentation(value);
	 * }
	 * </pre>
	 * 
	 * @return the value's string representation
	 */
	public abstract String getValueStringRepresentative();

	@Override
	public int hashCode() {
		return id.hashCode();
	}
}
