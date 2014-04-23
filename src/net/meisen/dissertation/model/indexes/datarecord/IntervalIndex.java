package net.meisen.dissertation.model.indexes.datarecord;

import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.util.Arrays;
import java.util.UUID;

import net.meisen.dissertation.exceptions.PersistorException;
import net.meisen.dissertation.model.data.IntervalModel;
import net.meisen.dissertation.model.data.TidaModel;
import net.meisen.dissertation.model.datasets.IDataRecord;
import net.meisen.dissertation.model.datastructure.IntervalStructureEntry;
import net.meisen.dissertation.model.indexes.BaseIndexFactory;
import net.meisen.dissertation.model.indexes.IRangeQueryOptimized;
import net.meisen.dissertation.model.indexes.datarecord.bitmap.Bitmap;
import net.meisen.dissertation.model.indexes.datarecord.slices.Slice;
import net.meisen.dissertation.model.indexes.datarecord.slices.SliceWithDescriptors;
import net.meisen.dissertation.model.persistence.BasePersistor;
import net.meisen.dissertation.model.persistence.Group;
import net.meisen.dissertation.model.persistence.Identifier;
import net.meisen.dissertation.model.time.mapper.BaseMapper;
import net.meisen.general.genmisc.exceptions.ForwardedRuntimeException;
import net.meisen.general.genmisc.types.Numbers;
import net.meisen.general.genmisc.types.Objects;

/**
 * An {@code IntervalIndex} is normally defined as an index for a mapper. The
 * size of the index is defined by a {@code Mapper}, which is used to map the
 * data-values to the underlying timeline.
 * 
 * @author pmeisen
 * 
 */
public class IntervalIndex implements IDataRecordIndex {
	private final static String EXTENSION = ".slice";

	private final IRangeQueryOptimized index;
	private final BaseMapper<?> mapper;
	private final BaseIndexFactory indexFactory;

	private Group persistentGroup = null;

	public IntervalIndex(final TidaModel model) {
		this(model.getIntervalModel());
	}

	/**
	 * Constructor to create an index using the specified {@code Mapper}, the
	 * {@code start}- and {@code end}-entry and the specified
	 * {@code indexFactory} to create the needed indexes.
	 */
	public IntervalIndex(final IntervalModel intervalModel) {
		this.mapper = intervalModel.getTimelineMapper();
		this.indexFactory = intervalModel.getIndexFactory();
		this.index = intervalModel.createIndex();
	}

	/**
	 * Gets the type of the identifiers used for the different slices.
	 * 
	 * @return the type of the identifiers
	 */
	public Class<? extends Number> getType() {
		return mapper.getTargetType();
	}

	/**
	 * Gets a readable version of the start.
	 * 
	 * @return a readable version of the start
	 */
	public String getFormattedStart() {
		return getFormattedId(mapper.getNormStartAsLong());
	}

	/**
	 * Formats an id of a slice of the interval's index nicely.
	 * 
	 * @param id
	 *            the identifier of a slice to be formatted
	 * 
	 * @return the formatted identifier
	 */
	public String getFormattedId(final Object id) {
		if (id instanceof Number) {
			return getFormattedId(((Number) id).longValue());
		} else {
			throw new IllegalArgumentException("Invalid identifier '" + id
					+ "'.");
		}
	}

	/**
	 * Formats an id of a slice of the interval's index nicely.
	 * 
	 * @param id
	 *            the identifier of a slice to be formatted
	 * 
	 * @return the formatted identifier
	 */
	public String getFormattedId(final long id) {
		final Object value = mapper.resolve(id);
		return mapper.format(value);
	}

	/**
	 * 
	 * @param value
	 *            the value to be formatted
	 * @return the formatted value
	 */
	public String getFormattedValue(final Object value) {
		return mapper.format(value);
	}

	/**
	 * Casts the slices of this instance to a {@code Slice} -array.
	 * 
	 * @param slices
	 *            the slices to be cast
	 * 
	 * @return the array as array of {@code IIndexDimensionSlice}
	 */
	protected SliceWithDescriptors<?>[] castSlices(final Object[] slices) {
		return Objects.castArray(slices, SliceWithDescriptors.class);
	}

	@Override
	public void index(final int dataId, final ProcessedDataRecord rec) {
		if (rec == null) {
			return;
		} else if (rec.getStart() < 0 || rec.getEnd() < 0) {
			return;
		}

		for (long i = rec.getStart(); i < rec.getEnd() + 1; i++) {
			final SliceWithDescriptors<?> slice = (SliceWithDescriptors<?>) index
					.getObject(i);
			if (slice == null) {
				index.addObject(createSlice(i, dataId));
			} else {
				slice.set(dataId);
			}
		}
	}

	/**
	 * Get the value of the record for the specified
	 * {@code IntervalStructureEntry}.
	 * 
	 * @param rec
	 *            the {@code DataRecord} to get the value for
	 * @param entry
	 *            the {@code IntervalStructureEntry} to get the value for
	 * 
	 * @return the retrieved value
	 */
	protected Object getValue(final IDataRecord rec,
			final IntervalStructureEntry entry) {
		final String name = entry.getName();

		// determine the value which is of interest for the record
		final Object value;
		if (name == null) {
			value = rec.getValue(entry.getPosition());
		} else {
			value = rec.getValue(name);
		}

		return value;
	}

	/**
	 * Gets the amounts of slices, i.e. different values within the index.
	 * 
	 * @return the amounts of slices
	 */
	public int getAmountOfSlices() {
		return index.size();
	}

	/**
	 * Get the slices for the specified {@code start} (included) to {@code end}
	 * (included).
	 * 
	 * @param start
	 *            the start point (included)
	 * @param end
	 *            the end point (included)
	 * 
	 * @return the slices, which might be {@code null} if no data is there yet
	 */
	public SliceWithDescriptors<?>[] getSlices(final long start, final long end) {
		return castSlices(index.getObjectsByStartAndEnd(start, end));
	}

	/**
	 * Gets the slices of the {@code IntervalIndex}.
	 * 
	 * @return the slices of the {@code IntervalIndex}
	 */
	public SliceWithDescriptors<?>[] getSlices() {
		return getSlices(index.getMinValue(), index.getMaxValue());
	}

	/**
	 * Gets a slice of the index, i.e. a bitmap which defines which records have
	 * the value of the specified slice set (i.e. {@code 1}) and which don't
	 * (i.e. {@code 0}).
	 * 
	 * @param point
	 *            the identifier of the value, i.e. the identifier of the value
	 *            of the index to retrieve the information for
	 * @return a bitmap with the identifiers of the records set to {@code 1} if
	 *         and only if the record's value is referred by the specified
	 *         {@code point}
	 */
	public SliceWithDescriptors<?> getSliceById(final long point) {
		return (SliceWithDescriptors<?>) index.getObject(point);
	}

	/**
	 * And-combines the slices for the specified {@code start} (included) to
	 * {@code end} (included).
	 * 
	 * @param start
	 *            the start point (included)
	 * @param end
	 *            the end point (included)
	 * 
	 * @return the result of the combination of the specified slices (by and)
	 */
	public Bitmap and(final long start, final long end) {
		return Bitmap.and(indexFactory,
				index.getObjectsByStartAndEnd(start, end));
	}

	/**
	 * Or-combines the slices for the specified {@code start} (included) to
	 * {@code end} (included).
	 * 
	 * @param start
	 *            the start point (included)
	 * @param end
	 *            the end point (included)
	 * 
	 * @return the result of the combination of the specified slices (by or)
	 */
	public Bitmap or(final long start, final long end) {
		return Bitmap.or(indexFactory,
				index.getObjectsByStartAndEnd(start, end));
	}

	/**
	 * Method to create a slice with the specified id and the specified set
	 * records.
	 * 
	 * @param sliceId
	 *            the identifier of the slice
	 * @param recordIds
	 *            the records to be marked as part of the slice
	 * 
	 * @return the created {@code Slice}
	 */
	protected SliceWithDescriptors<?> createSlice(final Number sliceId,
			final int... recordIds) {
		final Class<? extends Number> clazz = getType();

		// determine the slices types
		if (Byte.class.equals(clazz)) {
			return new SliceWithDescriptors<Byte>(Numbers.castToByte(sliceId),
					indexFactory, recordIds);
		} else if (Short.class.equals(clazz)) {
			return new SliceWithDescriptors<Short>(
					Numbers.castToShort(sliceId), indexFactory, recordIds);
		} else if (Integer.class.equals(clazz)) {
			return new SliceWithDescriptors<Integer>(
					Numbers.castToInt(sliceId), indexFactory, recordIds);
		} else {
			return new SliceWithDescriptors<Long>(Numbers.castToLong(sliceId),
					indexFactory, recordIds);
		}
	}

	/**
	 * Gets the interval dimensions for the specified {@code start} and
	 * {@code end} values.
	 * 
	 * @param start
	 *            the start of the values to be retrieved
	 * @param end
	 *            the end of the values to be retrieved
	 * @param startInclusive
	 *            {@code true} to define the start value to be inclusive,
	 *            otherwise it is exclusive
	 * @param endInclusive
	 *            {@code true} to define the start value to be inclusive,
	 *            otherwise it is exclusive
	 * 
	 * @return the slices between
	 */
	public SliceWithDescriptors<?>[] getSlices(final Object start,
			final Object end, final boolean startInclusive,
			final boolean endInclusive) {

		final long lStart = startInclusive ? mapper.mapToLong(start) : mapper
				.shiftToLong(start, 1, false);
		final long lEnd = endInclusive ? mapper.mapToLong(end) : mapper
				.shiftToLong(end, 1, true);
		return getSlices(lStart, lEnd);
	}

	@Override
	public void optimize() {
		for (final SliceWithDescriptors<?> slice : getSlices()) {
			if (slice != null) {
				slice.optimize();
			}
		}
	}

	@Override
	public void save(final BasePersistor persistor)
			throws ForwardedRuntimeException {

		for (final SliceWithDescriptors<?> slice : getSlices()) {
			if (slice == null) {
				continue;
			}

			// if we have a slice persist it
			final String id = UUID.randomUUID().toString() + EXTENSION;
			final Identifier identifier = new Identifier(id, persistentGroup);
			identifier.setComment(mapper.demap(
					Numbers.castToLong((Number) slice.getId())).toString());
			final OutputStream out = persistor.openForWrite(identifier);

			try {
				persistor.writeObject(out, slice.getId());
				slice.getBitmap().serialize(new DataOutputStream(out));
			} catch (final IOException e) {
				throw new ForwardedRuntimeException(PersistorException.class,
						1003, e, e.getMessage());
			}

			persistor.close(identifier);
		}
	}

	@Override
	public void load(final BasePersistor persistor,
			final Identifier identifier, final InputStream inputStream)
			throws ForwardedRuntimeException {

		// get the InputStream
		final Object id;
		try {
			id = persistor.readObject(inputStream);
		} catch (final Exception e) {
			throw new ForwardedRuntimeException(PersistorException.class, 1004,
					e, e.getMessage());
		}

		// check if the slice is already indexed
		if (index.getObject(id) != null) {
			throw new ForwardedRuntimeException(PersistorException.class, 1004,
					"The identifier '" + id + "' already exists.");
		} else if (id instanceof Number == false) {
			throw new ForwardedRuntimeException(PersistorException.class, 1004,
					"The identifier '" + id + "' is not a number.");
		}

		// create the slice
		final SliceWithDescriptors<?> slice = createSlice((Number) id);

		// load the slice from the InputStream
		try {
			slice.getBitmap().deserialize(new DataInputStream(inputStream));
		} catch (final IOException e) {
			throw new ForwardedRuntimeException(PersistorException.class, 1004,
					e, e.getMessage());
		}

		// add the slice
		index.addObject(slice);
	}

	@Override
	public void isRegistered(final BasePersistor persistor, final Group group) {
		this.persistentGroup = group;
	}

	@Override
	public Group getPersistentGroup() {
		return persistentGroup;
	}

	/**
	 * Gets the normalized start value of the index.
	 * 
	 * @return the normalized start value of the index
	 */
	public long getNormStart() {
		return mapper.getNormStartAsLong();
	}

	/**
	 * Gets the normalized end value of the index.
	 * 
	 * @return the normalized end value of the index
	 */
	public long getNormEnd() {
		return mapper.getNormEndAsLong();
	}

	/**
	 * Gets the value of the timeSlice positioned relatively from the specified
	 * {@code start}.
	 * 
	 * @param start
	 *            the start value within the timeline.
	 * @param startInclusive
	 *            {@code true} to define the start value to be inclusive,
	 *            otherwise it is exclusive
	 * @param pos
	 *            the - relative to the {@code start} - position to determine
	 *            the formatted value for
	 * 
	 * @return the formatted value
	 */
	public Object getValue(final Object start, final boolean startInclusive,
			final long pos) {

		// determine the position, i.e. the normalized value
		long valuePos = mapper.mapToLong(start);
		if (!startInclusive) {
			valuePos = valuePos + pos + 1;
		} else {
			valuePos = valuePos + pos;
		}

		return getValue(valuePos);
	}

	/**
	 * Determines the value associated to the specified {@code normalizedValue}.
	 * 
	 * @param normalizedValue
	 *            the value to get the associated object for
	 * 
	 * @return the object associated to the {@code normalizedValue}
	 */
	public Object getValue(final long normalizedValue) {
		return mapper.resolve(normalizedValue);
	}

	/**
	 * Gets the value for the specified {@code id}.
	 * 
	 * @param id
	 *            the identifier to get the associated value for
	 * 
	 * @return the associated value to the specified identifier
	 */
	public Object getValue(final Object id) {
		if (id instanceof Number) {
			return getValue(((Number) id).longValue());
		} else {
			throw new IllegalArgumentException("Invalid identifier '" + id
					+ "'.");
		}
	}
}
