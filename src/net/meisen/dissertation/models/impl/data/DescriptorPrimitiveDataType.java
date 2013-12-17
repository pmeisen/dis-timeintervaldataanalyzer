package net.meisen.dissertation.models.impl.data;

import net.meisen.general.genmisc.types.Objects;

/**
 * Primitive data types give a lot of performance chances, compared to the
 * default wrapper implementations. Therefore a {@code Descriptor} which is
 * based on a primitive data type.
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
public abstract class DescriptorPrimitiveDataType<D extends Object, T extends Descriptor<D, T, I>, I extends Object>
		extends Descriptor<D, T, I> {

	/**
	 * Just the default constructor of the {@code Descriptor}.
	 * 
	 * @param model
	 *            the {@code DescriptorModel} of the descriptor
	 * @param id
	 *            the identifier of the descriptor
	 */
	public DescriptorPrimitiveDataType(final DescriptorModel model, final I id) {
		super(model, id);
	}

	@Override
	public boolean equals(final Object o) {
		boolean cmp = false;

		if (o == this) {
			cmp = true;
		} else if (o == null) {
			// nothing to do
		} else if (getClass().equals(o.getClass())) {

			@SuppressWarnings("unchecked")
			final T d = (T) o;

			// check the values
			if (valueEquals(d) && Objects.equals(getModel(), d.getModel())) {
				cmp = true;
			}
		}

		return cmp;
	}

	/**
	 * Returns the Java class which is the representative for the primitive data
	 * type this {@code Descriptor} handles. It must be one of the following
	 * class:
	 * 
	 * <ul>
	 * <li>{@link Boolean}</li>
	 * <li>{@link Byte}</li>
	 * <li>{@link Short}</li>
	 * <li>{@link Integer}</li>
	 * <li>{@link Long}</li>
	 * <li>{@link Float}</li>
	 * <li>{@link Double}</li>
	 * <li>{@link Character}</li>
	 * </ul>
	 * 
	 * @return the Java class which is the representative for the primitive data
	 *         type
	 */
	public abstract Class<D> getPrimitiveDataType();

	/**
	 * Method which must be implemented to compare the primitive types. It is
	 * ensured that the passed {@code descriptor} will not be {@code null}.
	 * 
	 * @param descriptor
	 *            the descriptor to compare the values for
	 * 
	 * @return {@code true} if the values are equal, otherwise {@code false}
	 */
	public abstract boolean valueEquals(final T descriptor);
}
