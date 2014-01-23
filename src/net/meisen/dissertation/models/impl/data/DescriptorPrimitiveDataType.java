package net.meisen.dissertation.models.impl.data;

import java.math.BigDecimal;
import java.math.BigInteger;

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
	public DescriptorPrimitiveDataType(final DescriptorModel<I> model, final I id) {
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
	 * Maps the specified value to the primitive data type used by {@code this}
	 * descriptor.
	 * 
	 * @param value
	 *            the value to be mapped
	 * 
	 * @return the mapped value or {@code null} if it cannot be mapped to the
	 *         primitive type
	 */
	public D mapToDataType(final Object value) {

		if (value == null) {
			return null;
		}

		final Class<D> clazz = getPrimitiveDataType();

		if (clazz.equals(value.getClass())) {
			return castToType(value);
		} else if (Number.class.isAssignableFrom(clazz)) {
			@SuppressWarnings("unchecked")
			final Class<? extends Number> numClazz = (Class<? extends Number>) clazz;
			final Number number = (Number) value;
			final Number result = castToNumber(number, numClazz);

			// check the result
			if (result != null) {
				final Class<?> srcClazz = number.getClass();
				final Number cmpNumber = castToNumber(result, number.getClass());

				if (cmpNumber.equals(number)) {
					return castToType(result);
				}
				/*
				 * There is a problem with the BigDecimal the equality depends
				 * on how it is created, i.e. using new BigDecimal(...) or
				 * BigDecimal.valueOf(...). The castToNumber method uses the
				 * valueOf, therefore here we check the constructor.
				 */
				else if (BigDecimal.class.equals(srcClazz)
						&& new BigDecimal(result.doubleValue()).equals(number)) {
					return castToType(result);
				}
			}
		}

		// if we came so far there is no hope
		return null;
	}

	/**
	 * Method which maps a {@code number} to the specified {@code clazz}. The
	 * specified {@code clazz} is another {@code Number}.
	 * 
	 * @param number
	 *            the number to be casted
	 * @param clazz
	 *            the {@code Number}-class to cast the {@code number} to
	 * 
	 * @return the casted {@code number} or {@code null} if a cast wasn't
	 *         possible
	 */
	protected Number castToNumber(final Number number,
			final Class<? extends Number> clazz) {
		final Number result;

		if (number == null) {
			return null;
		} else if (number.getClass().equals(clazz)) {
			return number;
		} else if (Byte.class.equals(clazz)) {
			result = number.byteValue();
		} else if (Short.class.equals(clazz)) {
			result = number.shortValue();
		} else if (Integer.class.equals(clazz)) {
			result = number.intValue();
		} else if (Long.class.equals(clazz)) {
			result = number.longValue();
		} else if (Float.class.equals(clazz)) {
			result = number.floatValue();
		} else if (Double.class.equals(clazz)) {
			result = number.doubleValue();
		} else if (BigInteger.class.equals(clazz)) {
			result = BigInteger.valueOf(number.longValue());
		} else if (BigDecimal.class.equals(clazz)) {
			result = BigDecimal.valueOf(number.doubleValue());
		} else {
			return null;
		}

		return result;
	}

	/**
	 * Helper method to cast a value to the specified type.
	 * 
	 * @param value
	 *            the value to be casted
	 *            
	 * @return the casted value
	 */
	protected D castToType(final Object value) {
		@SuppressWarnings("unchecked")
		final D result = (D) value;
		return result;
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
