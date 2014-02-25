package net.meisen.dissertation.model.time.mapper;

import net.meisen.dissertation.model.time.granularity.ITimeGranularity;
import net.meisen.general.genmisc.types.Numbers;

/**
 * Default implementation of a {@code Mapper} which is used to map data of type
 * {@code T} to a {@code long}-value.
 * 
 * @author pmeisen
 * 
 * @param <T>
 *            the type to map from
 */
public abstract class BaseMapper<T> {
	private final long start;
	private final long end;
	private final long distance;
	private final Class<?> targetType;
	private final ITimeGranularity granularity;

	/**
	 * Generates a {@code Mapper} which is used to map values of the type
	 * {@code T} to a {@code long}, {@code int}, {@code short}, or {@code byte}
	 * depending on the range of the values to map to (i.e. {@code end - start}
	 * ). This method calls the implementation of {@link #map(Object)} so be
	 * sure that this method only uses attributes set so far (i.e. only the
	 * {@link #getGranularity()} is set) or set others using
	 * {@link #initPriorToMap(Object, Object, ITimeGranularity, Object...)}.
	 * 
	 * @param start
	 *            the minimal value for the {@code Mapper} (included)
	 * @param end
	 *            the maximum value for the {@code Mapper} (included)
	 * @param granularity
	 *            the granularity of the mapping, i.e. which information can get
	 *            truncated
	 * @param params
	 *            parameters which are ignored by the default implementation,
	 *            but send to
	 *            {@link #initPriorToMap(Object, Object, ITimeGranularity, Object...)}
	 *            and can be used by the concrete one
	 * 
	 * @throws IllegalArgumentException
	 *             the exception is thrown if the mapped {@code end} value is
	 *             smaller than the mapped {@code start} value
	 */
	public BaseMapper(final T start, final T end,
			final ITimeGranularity granularity, final Object... params) {
		this.granularity = granularity;
		initPriorToMap(start, end, granularity, params);

		// set the start and the end
		this.start = map(start);
		this.end = map(end);
		if (this.end < this.start) {
			throw new IllegalArgumentException(
					"The end cannot be larger than start.");
		}

		// this should even work correctly for negative values
		this.distance = this.end - this.start;

		// determine the range to be used
		this.targetType = determineType(this.start, this.end);
	}

	/**
	 * Helper method to set attributes prior to the super constructor call of
	 * {@link #map(Object)} triggered by the constructor
	 * {@link #BaseMapper(Object, Object, ITimeGranularity, Object[])}.
	 * 
	 * @param start
	 *            the unmapped start value
	 * @param end
	 *            the unmapped end value
	 * @param granularity
	 *            the granularity
	 * @param params
	 *            the parameters passed to the constructor as such
	 */
	protected void initPriorToMap(final T start, final T end,
			final ITimeGranularity granularity, final Object... params) {
		// do nothing
	}

	/**
	 * Generates a {@code Mapper} which is used to map values of the type
	 * {@code T} to a {@code long}, {@code int}, {@code short}, or {@code byte}
	 * depending on the range of the values to map to (i.e. {@code end - start}
	 * ).
	 * 
	 * @param start
	 *            the minimal value for the {@code Mapper} (included)
	 * @param end
	 *            the maximum value for the {@code Mapper} (included)
	 * @param granularity
	 *            the granularity of the mapping, i.e. which information can get
	 *            truncated
	 * 
	 * @throws IllegalArgumentException
	 *             the exception is thrown if the {@code end} value is smaller
	 *             than the {@code start} value
	 */
	public BaseMapper(final long start, final long end,
			final ITimeGranularity granularity) throws IllegalArgumentException {
		if (end < start) {
			throw new IllegalArgumentException(
					"The end cannot be larger than start.");
		}

		this.granularity = granularity;

		// set the start and the end
		this.start = start;
		this.end = end;

		// this should even work correctly for negative values
		this.distance = end - start;

		// determine the range to be used
		this.targetType = determineType(start, end);
	}

	/**
	 * Calculates the distance between {@code start} and {@code end} and
	 * determines which primitive datatype can be used to hold such a value.
	 * 
	 * @param start
	 *            the start value
	 * @param end
	 *            the end value
	 * 
	 * @return the primitive datatype
	 */
	protected Class<?> determineType(final long start, final long end) {
		long distance = end - start;

		// determine the range to be used
		if (distance <= Byte.MAX_VALUE) {
			return Byte.class;
		} else if (distance <= Short.MAX_VALUE) {
			return Short.class;
		} else if (distance <= Integer.MAX_VALUE) {
			return Integer.class;
		} else {
			return Long.class;
		}
	}

	/**
	 * Gets the type which the mapper maps from.
	 * 
	 * @return the type which the mapper maps from
	 */
	public abstract Class<T> getMappedType();

	/**
	 * The target type which is normally a {@code long} but it may be possible
	 * (if the range is small enough) that a smaller data-type can be used, i.e.
	 * a {@code byte}, {@code short}, or {@code int}. Therefore the
	 * {@code Mapper} offers some methods, those are:
	 * <ul>
	 * <li>{@code getValueStartAs[Type]()}</li>
	 * <li>{@code getValueEndAs[Type]()}</li>
	 * <li>{@code mapTo[Type](T)}</li>
	 * <li>{@code resolve([Type])}</li>
	 * </ul>
	 * 
	 * @return the type to be used best, if one of the methods is called without
	 *         being appropriate, an error will most likely be thrown
	 */
	public Class<?> getTargetType() {
		return targetType;
	}

	/**
	 * Map the specified {@code from} to a {@code byte}. The method returns a
	 * normalized value.<br/>
	 * <br/>
	 * <b>Note:</b><br/>
	 * The method might throw an exception if the {@link #getTargetType()} is
	 * not {@code Byte.class}.
	 * 
	 * @param from
	 *            the data to be mapped
	 * 
	 * @return the mapped value (as byte)
	 */
	public byte mapToByte(final Object from) {
		return Numbers.castToByte(mapToLong(from));
	}

	/**
	 * Map the specified {@code from} to a {@code short}. The method returns a
	 * normalized value.<br/>
	 * <br/>
	 * <b>Note:</b><br/>
	 * The method might throw an exception if the {@link #getTargetType()} is
	 * not {@code Short.class}.
	 * 
	 * @param from
	 *            the data to be mapped
	 * 
	 * @return the mapped value (as short)
	 */
	public short mapToShort(final Object from) {
		return Numbers.castToShort(mapToLong(from));
	}

	/**
	 * Map the specified {@code from} to a {@code int}. The method returns a
	 * normalized value.<br/>
	 * <br/>
	 * <b>Note:</b><br/>
	 * The method might throw an exception if the {@link #getTargetType()} is
	 * not {@code Integer.class}.
	 * 
	 * @param from
	 *            the data to be mapped
	 * 
	 * @return the mapped value (as int)
	 */
	public int mapToInt(final Object from) {
		return Numbers.castToInt(mapToLong(from));
	}

	/**
	 * Map the specified {@code from} to a {@code long}. The method returns a
	 * normalized value.
	 * 
	 * @param from
	 *            the data to be mapped
	 * 
	 * @return the mapped value (as long)
	 */
	public long mapToLong(final Object from) {
		return normalize(map(validate(from)));
	}

	/**
	 * Validates the specified {@code o} to be a valid object to be mapped by
	 * {@code this}. The default implementation checks if the object is of the
	 * type specified by {@link #getTargetType()}.
	 * 
	 * @param o
	 *            the object to be checked
	 * @return the object of a type which can be mapped
	 * 
	 * @throws NullPointerException
	 *             if the {@code o} is {@code null}
	 * @throws IllegalArgumentException
	 *             if the specified {@code o} cannot be mapped
	 */
	@SuppressWarnings("unchecked")
	protected T validate(final Object o) throws IllegalArgumentException,
			NullPointerException {
		if (o == null) {
			throw new NullPointerException("The null-value cannot be mapped.");
		} else if (getMappedType().isAssignableFrom(o.getClass())) {
			return (T) o;
		} else {
			throw new IllegalArgumentException("The class '"
					+ getMappedType().getName() + "' is not assignable from '"
					+ o.getClass().getName() + "'.");
		}
	}

	/**
	 * This method contains the concrete mapping implementation of the
	 * {@code Mapper}. It implements the mapping from {@code from} to a
	 * specified {@code long} value.
	 * 
	 * @param from
	 *            the value to be mapped
	 * 
	 * @return the mapped value
	 */
	protected abstract long map(final T from);

	/**
	 * Internally used to normalize the mapped result to an internal
	 * representation, i.e. the value is 0-based concerning the start-value.<br/>
	 * <br/>
	 * <b>Note:</b><br/>
	 * This method should always be called prior to {@link #map(Object)}. The
	 * methods {@link #mapToByte(Object)}, {@link #mapToShort(Object)},
	 * {@link #mapToInt(Object)}, and {@link #mapToLong(Object)} call this
	 * method prior to calling {@link #map(Object)} therefore this method
	 * doesn't have to be when using one of those methods.
	 * 
	 * @param mappedValue
	 *            the result of the mapping process
	 * 
	 * @return the normalized value which can be internally used for indexing
	 */
	protected long normalize(final long mappedValue) {
		if (mappedValue < getStart()) {
			return getNormStartAsLong();
		} else if (mappedValue > getEnd()) {
			return getNormEndAsLong();
		} else {
			return mappedValue - getStart();
		}
	}

	/**
	 * Method used to denormalize a normalized value. <br/>
	 * <br/>
	 * <b>Note:</b><br/>
	 * This method should always be called prior to {@link #demap(long)}. The
	 * methods {@link #resolve(byte)}, {@link #resolve(short)},
	 * {@link #resolve(int)}, and {@link #resolve(long)} call this method prior
	 * to calling {@link #demap(long)} therefore this method doesn't have to be
	 * when using one of those methods.
	 * 
	 * @param normalizedValue
	 *            the normalized value
	 * 
	 * @return the denormalized value which can be resolved
	 */
	protected long denormalize(final long normalizedValue) {
		if (normalizedValue < 0l) {
			return getStart();
		} else {
			final long denormalizedValue = normalizedValue + getStart();
			return denormalizedValue > getEnd() ? getEnd() : denormalizedValue;
		}
	}

	/**
	 * Resolves the specified normalized value (i.e. the value which was
	 * returned by {@link #mapToByte(Object)}.
	 * 
	 * @param value
	 *            the normalized byte value
	 * 
	 * @return the resolved value
	 */
	public T resolve(final byte value) {
		return resolve((long) value);
	}

	/**
	 * Resolves the specified normalized value (i.e. the value which was
	 * returned by {@link #mapToShort(Object)}.
	 * 
	 * @param value
	 *            the normalized short value
	 * 
	 * @return the resolved value
	 */
	public T resolve(final short value) {
		return resolve((long) value);
	}

	/**
	 * Resolves the specified normalized value (i.e. the value which was
	 * returned by {@link #mapToInt(Object)}.
	 * 
	 * @param value
	 *            the normalized int value
	 * 
	 * @return the resolved value
	 */
	public T resolve(final int value) {
		return resolve((long) value);
	}

	/**
	 * Resolves the specified normalized value (i.e. the value which was
	 * returned by {@link #mapToLong(Object)}.
	 * 
	 * @param value
	 *            the normalized long value
	 * 
	 * @return the resolved value
	 */
	public T resolve(final long value) {
		return demap(denormalize(value));
	}

	/**
	 * This is the concrete implementation of the demapping used to map a
	 * normalized value to the concrete value.
	 * 
	 * @param value
	 *            the denormalized value
	 * 
	 * @return the concrete value
	 */
	public abstract T demap(final long value);

	/**
	 * Gets the denormalized start value.
	 * 
	 * @return the denormalized start value
	 */
	public long getStart() {
		return start;
	}

	/**
	 * Gets the denormalized end value.
	 * 
	 * @return the denormalized end value
	 */
	public long getEnd() {
		return end;
	}

	/**
	 * Gets the normalized start value (as {@code byte}).
	 * 
	 * @return the normalized start value (as {@code byte})
	 */
	public byte getNormStartAsByte() {
		return Numbers.castToByte(getNormStartAsLong());
	}

	/**
	 * Gets the normalized start value (as {@code short}).
	 * 
	 * @return the normalized start value (as {@code short})
	 */
	public short getNormStartAsShort() {
		return Numbers.castToShort(getNormStartAsLong());
	}

	/**
	 * Gets the normalized start value (as {@code int}).
	 * 
	 * @return the normalized start value (as {@code int})
	 */
	public int getNormStartAsInt() {
		return Numbers.castToInt(getNormStartAsLong());
	}

	/**
	 * Gets the normalized start value (as {@code long}).
	 * 
	 * @return the normalized start value (as {@code long})
	 */
	public long getNormStartAsLong() {
		return 0l;
	}

	/**
	 * Gets the normalized end value (as {@code byte}).
	 * 
	 * @return the normalized end value (as {@code byte})
	 */
	public byte getNormEndAsByte() {
		return Numbers.castToByte(getNormEndAsLong());
	}

	/**
	 * Gets the normalized end value (as {@code short}).
	 * 
	 * @return the normalized end value (as {@code short})
	 */
	public short getNormEndAsShort() {
		return Numbers.castToShort(getNormEndAsLong());
	}

	/**
	 * Gets the normalized end value (as {@code int}).
	 * 
	 * @return the normalized end value (as {@code int})
	 */
	public int getNormEndAsInt() {
		return Numbers.castToInt(getNormEndAsLong());
	}

	/**
	 * Gets the normalized end value (as {@code long}).
	 * 
	 * @return the normalized end value (as {@code long})
	 */
	public long getNormEndAsLong() {
		return distance;
	}

	@Override
	public String toString() {
		return "Mapper for values between '" + demap(getStart()) + "' - '"
				+ demap(getEnd()) + "'";
	}

	/**
	 * Gets the granularity specified for the {@code Mapper}.
	 * 
	 * @return the granularity specified for the {@code Mapper}
	 */
	public ITimeGranularity getGranularity() {
		return granularity;
	}
}