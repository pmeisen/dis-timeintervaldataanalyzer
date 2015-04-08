package net.meisen.dissertation.model.time.mapper;

import java.util.Iterator;

import net.meisen.dissertation.impl.parser.query.Interval;
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
	private final Class<? extends Number> targetType;
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
	protected Class<? extends Number> determineType(final long start,
			final long end) {
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
	public Class<? extends Number> getTargetType() {
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
	 * Map the specified {@code from} to an {@code int}. The method returns a
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
	 * Checks if the value specified by {@code from} is smaller than the actual
	 * end of the timeline.
	 * 
	 * @param from
	 *            the data to be checked
	 * 
	 * @return {@code true} if the value undercuts the start of the timeline,
	 *         otherwise {@code false}
	 */
	public boolean isSmallerThanStart(final Object from) {
		final long mappedValue = map(validate(from));
		return mappedValue < getStart();
	}

	/**
	 * Checks if the value specified by {@code from} is larger than the actual
	 * end of the timeline.
	 * 
	 * @param from
	 *            the data to be checked
	 * 
	 * @return {@code true} if the value exceeds the end of the timeline,
	 *         otherwise {@code false}
	 */
	public boolean isLargerThanEnd(final Object from) {
		final long mappedValue = map(validate(from));
		return mappedValue > getEnd();
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
		} else if (String.class.equals(o.getClass())) {
			return resolve((String) o);
		} else {
			throw new IllegalArgumentException("The class '"
					+ getMappedType().getName() + "' is not assignable from '"
					+ o.getClass().getName() + "'.");
		}
	}

	/**
	 * This method contains the concrete mapping implementation of the
	 * {@code Mapper}. It implements the mapping from {@code from} to a
	 * specified {@code long} value, which is called <b>denormalized</b> value.
	 * 
	 * @param from
	 *            the value to be mapped
	 * 
	 * @return the mapped value
	 */
	protected abstract long map(final T from);

	/**
	 * Method used to resolve a string to a value of the mapper's type.
	 * 
	 * @param value
	 *            the value to be parsed
	 * 
	 * @return the resolved value
	 * 
	 * @throws IllegalArgumentException
	 *             if the value cannot be parsed
	 */
	protected abstract T resolve(final String value)
			throws IllegalArgumentException;

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
	 * This method is used to retrieve the <b>normalized</b> shifted value for
	 * the specified {@code value}.
	 * 
	 * @param value
	 *            the value to be shifted and normalized
	 * @param steps
	 *            a positive value to determine how many steps should be shifted
	 * @param towardsStart
	 *            the direction to shift the value to, i.e. {@code true} means
	 *            that the value will be shifted towards the start, otherwise
	 *            {@code false} towards the end
	 * 
	 * @return the shifted value, whereby the edges are recognized
	 */
	public long shiftToLong(final Object value, final int steps,
			final boolean towardsStart) {
		final long mappedValue = map(validate(value));

		final long normValue;
		if (towardsStart) {
			if (mappedValue > getStart()) {
				normValue = normalize(mappedValue - steps);
			} else {
				normValue = getNormStartAsLong();
			}
		} else {
			if (mappedValue < getEnd()) {
				normValue = normalize(mappedValue + steps);
			} else {
				normValue = getNormEndAsLong();
			}
		}

		return normValue;
	}

	/**
	 * This method is used to retrieve the <b>normalized</b> shifted value for
	 * the specified {@code value}.
	 * 
	 * @param value
	 *            the value to be shifted and normalized
	 * @param steps
	 *            a positive value to determine how many steps should be shifted
	 * @param towardsStart
	 *            the direction to shift the value to, i.e. {@code true} means
	 *            that the value will be shifted towards the start, otherwise
	 *            {@code false} towards the end
	 * 
	 * @return the shifted value, whereby the edges are recognized
	 */
	public byte shiftToByte(final Object value, final int steps,
			final boolean towardsStart) {
		return Numbers.castToByte(shiftToLong(value, steps, towardsStart));
	}

	/**
	 * This method is used to retrieve the <b>normalized</b> shifted value for
	 * the specified {@code value}.
	 * 
	 * @param value
	 *            the value to be shifted and normalized
	 * @param steps
	 *            a positive value to determine how many steps should be shifted
	 * @param towardsStart
	 *            the direction to shift the value to, i.e. {@code true} means
	 *            that the value will be shifted towards the start, otherwise
	 *            {@code false} towards the end
	 * 
	 * @return the shifted value, whereby the edges are recognized
	 */
	public short shiftToShort(final Object value, final int steps,
			final boolean towardsStart) {
		return Numbers.castToShort(shiftToLong(value, steps, towardsStart));
	}

	/**
	 * This method is used to retrieve the <b>normalized</b> shifted value for
	 * the specified {@code value}.
	 * 
	 * @param value
	 *            the value to be shifted and normalized
	 * @param steps
	 *            a positive value to determine how many steps should be shifted
	 * @param towardsStart
	 *            the direction to shift the value to, i.e. {@code true} means
	 *            that the value will be shifted towards the start, otherwise
	 *            {@code false} towards the end
	 * 
	 * @return the shifted value, whereby the edges are recognized
	 */
	public int shiftToInt(final Object value, final int steps,
			final boolean towardsStart) {
		return Numbers.castToInt(shiftToLong(value, steps, towardsStart));
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
	 * returned by {@link #mapToLong(Object)}).
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
	 * <b>denormalize</b> value to the concrete value.
	 * 
	 * @param value
	 *            the denormalized value
	 * 
	 * @return the concrete value
	 * 
	 * @see #denormalize(long)
	 * @see #resolve(long)
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

	/**
	 * Formats the {@code value} to a readable value. This method gets the real
	 * value (i.e. the source-value). Use {@link #resolve(long)} to resolve an
	 * identifier to it's real value prior to calling this method.
	 * 
	 * @param value
	 *            the source-value to be mapped to a formatted value
	 * 
	 * @return the formatted value
	 */
	@SuppressWarnings("unchecked")
	public String format(final Object value) {
		if (value == null) {
			return null;
		} else if (getMappedType().isAssignableFrom(value.getClass())) {
			return formatValue((T) value);
		} else {
			throw new IllegalArgumentException("Invalid value '" + value
					+ "' for mapper '" + getClass().getName() + "'.");
		}
	}

	/**
	 * Formats the value to a string for output.
	 * 
	 * @param value
	 *            the value to be formatted
	 * 
	 * @return the formatted value
	 */
	protected String formatValue(final T value) {
		return value == null ? null : value.toString();
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

	/**
	 * Determines the bounds specified by the passed values, i.e.
	 * {@code [start, end]}.
	 * 
	 * @param start
	 *            the start object
	 * @param end
	 *            the end object
	 * 
	 * @return the determined bounds
	 */
	public long[] getBounds(final Object start, final Object end) {
		return getBounds(start, end, true, true);
	}

	/**
	 * Determines the bounds specified by the passed values.
	 * 
	 * @param start
	 *            the start object
	 * @param end
	 *            the end object
	 * @param startInclusive
	 *            {@code true} if the start value is included, otherwise
	 *            {@code false}
	 * @param endInclusive
	 *            {@code true} if the end value is included, otherwise
	 *            {@code false}
	 * 
	 * @return the determined bounds or {@code null} if the specified
	 *         {@code start} is outside the {@code timeline's end} or
	 *         {@code the end} is outside the {@code timeline's start}
	 */
	public long[] getBounds(final Object start, final Object end,
			final boolean startInclusive, final boolean endInclusive) {

		// check if the values are out of bound
		if (isLargerThanEnd(start)) {
			return null;
		} else if (isSmallerThanStart(end)) {
			return null;
		}

		// get the mapped values
		final long lStart = startInclusive ? mapToLong(start) : shiftToLong(
				start, 1, false);
		final long lEnd = endInclusive ? mapToLong(end) : shiftToLong(end, 1,
				true);

		return new long[] { lStart, lEnd };
	}

	/**
	 * Determines the bounds specified by the {@code Interval}.
	 * 
	 * @param interval
	 *            the interval to determine the bound for
	 * 
	 * @return the determined bounds, or {@code null} if the interval is
	 *         {@code null}, the specified {@code start} is outside the
	 *         {@code timeline's end} or {@code the end} is outside the
	 *         {@code timeline's start}
	 */
	public long[] getBounds(final Interval<?> interval) {
		if (interval == null) {
			return null;
		} else {
			return getBounds(interval.getStart(), interval.getEnd(), interval
					.getOpenType().isInclusive(), interval.getCloseType()
					.isInclusive());
		}
	}

	/**
	 * This method is used to create an iterator, iterating over the time-line
	 * in partitions defined. The method ensures, that the {@code interval} is a
	 * part of the partition. Each partition will contain the same amount of
	 * time-points as covered by the interval.
	 * 
	 * @param interval
	 *            the interval to create the iterator for
	 * 
	 * @return the partition iterator
	 */
	public Iterator<long[]> createTimelinePartitionIterator(
			final Interval<?> interval) {
		final long[] bounds = getBounds(interval);
		return createTimelinePartitionIterator(bounds);
	}

	/**
	 * This method is used to create an iterator, iterating over the time-line
	 * in partitions defined. The method ensures, that the {@code interval} is a
	 * part of the partition. Each partition will contain the same amount of
	 * time-points as covered by the interval.
	 * 
	 * @param range
	 *            the range to determine the iterator for
	 * 
	 * @return the partition iterator
	 */
	public Iterator<long[]> createTimelinePartitionIterator(final long[] range) {

		final long windowStart = range[0];
		final long windowEnd = range[1];

		if (windowEnd - windowStart > distance) {
			return new Iterator<long[]>() {

				@Override
				public boolean hasNext() {
					return false;
				}

				@Override
				public long[] next() {
					return null;
				}

				@Override
				public void remove() {
					// not supported, silently ignored
				}
			};
		}

		final long windowSize = windowEnd - windowStart + 1;

		final long offset = (windowStart - getNormStartAsLong()) % windowSize;
		final long stepSize = windowSize - 1;

		final long firstStart;
		final long firstEnd;
		if (offset == 0) {
			firstStart = getNormStartAsLong();
			firstEnd = stepSize;
		} else {
			firstStart = offset + 1;
			firstEnd = Math.min(firstStart + stepSize, getNormEndAsLong());
		}

		return new Iterator<long[]>() {
			private long nextStart = firstStart;
			private long nextEnd = firstEnd;

			@Override
			public boolean hasNext() {
				return nextEnd - nextStart == stepSize;
			}

			@Override
			public long[] next() {
				final long[] next = new long[] { nextStart, nextEnd };

				nextStart = nextEnd + 1;
				nextEnd = Math.min(nextStart + stepSize, getNormEndAsLong());

				return next;
			}

			@Override
			public void remove() {
				// not supported, silently ignored
			}
		};
	}
}
