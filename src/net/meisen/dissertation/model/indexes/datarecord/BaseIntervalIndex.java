package net.meisen.dissertation.model.indexes.datarecord;

import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.util.UUID;

import net.meisen.dissertation.exceptions.PersistorException;
import net.meisen.dissertation.model.datasets.IDataRecord;
import net.meisen.dissertation.model.datastructure.IntervalStructureEntry;
import net.meisen.dissertation.model.indexes.BaseIndexFactory;
import net.meisen.dissertation.model.indexes.IRangeQueryOptimized;
import net.meisen.dissertation.model.indexes.IndexKeyDefinition;
import net.meisen.dissertation.model.indexes.datarecord.slices.IndexDimensionSlice;
import net.meisen.dissertation.model.persistence.BasePersistor;
import net.meisen.dissertation.model.persistence.Group;
import net.meisen.dissertation.model.persistence.Identifier;
import net.meisen.dissertation.model.time.mapper.BaseMapper;
import net.meisen.general.genmisc.exceptions.ForwardedRuntimeException;
import net.meisen.general.genmisc.types.Numbers;
import net.meisen.general.genmisc.types.Objects;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * An {@code IntervalIndex} is normally defined as an index for a mapper. The
 * size of the index is defined by a {@code Mapper}, which is used to map the
 * data-values to the underlying timeline.
 * 
 * @author pmeisen
 * 
 */
public abstract class BaseIntervalIndex implements IDataRecordIndex {
	private final static Logger LOG = LoggerFactory
			.getLogger(BaseIntervalIndex.class);
	private final static String EXTENSION = ".slice";

	@SuppressWarnings("rawtypes")
	private final BaseMapper mapper;

	private final IntervalStructureEntry startEntry;
	private final IntervalStructureEntry endEntry;
	private final IRangeQueryOptimized index;
	private final BaseIndexFactory indexFactory;

	private IntervalDataHandling intervalDataHandling;
	private Group persistentGroup = null;

	/**
	 * Constructor to create an index using the specified {@code Mapper}, the
	 * {@code start}- and {@code end}-entry and the specified
	 * {@code indexFactory} to create the needed indexes.
	 * 
	 * @param mapper
	 *            the {@code Mapper} which defines the start and end value, as
	 *            well as the type of the indexed values
	 * @param start
	 *            the {@code entry} which defines the start
	 * @param end
	 *            the {@code entry} which defines the end
	 * @param indexFactory
	 *            the {@code IndexFactory} to create the needed indexes
	 */
	public BaseIntervalIndex(final BaseMapper<?> mapper,
			final IntervalStructureEntry start,
			final IntervalStructureEntry end,
			final BaseIndexFactory indexFactory) {
		if (LOG.isTraceEnabled()) {
			LOG.trace("Creating " + getClass().getSimpleName() + " for '"
					+ mapper.format(mapper.demap(mapper.getStart())) + " - "
					+ mapper.format(mapper.demap(mapper.getEnd()))
					+ "' using granularity '" + mapper.getGranularity()
					+ "'...");
		}

		// set the entries
		this.startEntry = start;
		this.endEntry = end;

		// set the mapper
		this.mapper = mapper;

		// set the factory
		this.indexFactory = indexFactory;

		// create an index to handle the different values for a descriptor
		final IndexKeyDefinition indexKeyDef = new IndexKeyDefinition(
				IndexDimensionSlice.class, "getId");
		indexKeyDef.overrideType(0, getType());
		this.index = indexFactory.createRangeQueryOptimized(indexKeyDef);

		// the maximum value of the index is defined by the mapper
		this.index.setMaxValue(this.mapper.getNormEndAsLong());

		// set the default intervalDataHandling
		setIntervalDataHandling(null);

		// log the successful creation
		if (LOG.isTraceEnabled()) {
			LOG.trace("Created " + getClass().getSimpleName() + " for '"
					+ mapper.format(mapper.demap(mapper.getStart())) + " - "
					+ mapper.format(mapper.demap(mapper.getEnd()))
					+ "' with index '" + this.index.getClass().getName()
					+ "' for identifiers of the intervalIndex of type '"
					+ getType().getName() + "'.");
		}
	}

	/**
	 * Gets a readable version of the start.
	 * 
	 * @return a readable version of the start
	 */
	public String getFormattedStart() {
		return getFormattedId(getMapper().getNormStartAsLong());
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
		final Object value = getMapper().resolve(id);
		return getMapper().format(value);
	}

	/**
	 * 
	 * @param value
	 *            the value to be formatted
	 * @return the formatted value
	 */
	public String getFormattedValue(final Object value) {
		return getMapper().format(value);
	}

	/**
	 * Casts the slices of this instance to a {@code IIndexDimensionSlice}
	 * -array.
	 * 
	 * @param slices
	 *            the slices to be cast
	 * 
	 * @return the array as array of {@code IIndexDimensionSlice}
	 */
	protected IndexDimensionSlice<?>[] castSlices(final Object[] slices) {
		return Objects.castArray(slices, IndexDimensionSlice.class);
	}

	@Override
	public void index(final int dataId, final IDataRecord rec) {
		if (rec == null) {
			return;
		}

		// get the start and the end value of the record
		Object start = getValue(rec, getStartEntry());
		Object end = getValue(rec, getEndEntry());

		// check if there are null values
		final boolean nullValues = start == null || end == null;

		if (nullValues) {
			final IntervalDataHandling handling = getIntervalDataHandling();
			if (IntervalDataHandling.FAILONNULL.equals(handling)) {
				throw new NullPointerException(
						"Configuration does not allow null values within an interval.");
			} else if (IntervalDataHandling.USEOTHER.equals(handling)) {

				// set the values to be equal
				start = start == null ? end : start;
				end = end == null ? start : end;
			}
		}

		index(dataId, start, end);
	}

	/**
	 * Method to index the specified {@code dataId} for the specified
	 * {@code start} and {@code end} value. The implementation might still have
	 * to handle the {@code IntervalDataHandling}. Generally if {@code null}
	 * values are passed the handling could not be solved prior to this method.
	 * 
	 * @param dataId
	 *            the id of the record to be added
	 * @param start
	 *            the start value
	 * @param end
	 *            the end value
	 */
	protected abstract void index(final int dataId, final Object start,
			final Object end);

	/**
	 * Gets the type of the underlying timeline.
	 * 
	 * @return the type of the underlying timeline
	 */
	public Class<?> getType() {
		return mapper.getTargetType();
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
	 * Gets the slices of the {@code IntervalIndex}.
	 * 
	 * @return the slices of the {@code IntervalIndex}
	 */
	public abstract IndexDimensionSlice<?>[] getSlices();

	/**
	 * Method to create a slice with the specified id and the specified set
	 * records.
	 * 
	 * @param sliceId
	 *            the identifier of the slice
	 * @param recordIds
	 *            the records to be marked as part of the slice
	 * 
	 * @return the created {@code IndexDimensionSlice}
	 */
	protected abstract IndexDimensionSlice<?> createSlice(final Number sliceId,
			final int... recordIds);

	/**
	 * Get the defined {@code IntervalDataHandling}.
	 * 
	 * @return the defined {@code IntervalDataHandling}
	 * 
	 * @see IntervalDataHandling
	 */
	public IntervalDataHandling getIntervalDataHandling() {
		return intervalDataHandling;
	}

	/**
	 * Set the {@code IntervalDataHandling}.
	 * 
	 * @param intervalDataHandling
	 *            the {@code IntervalDataHandling}
	 * 
	 * @see IntervalDataHandling
	 */
	public void setIntervalDataHandling(
			final IntervalDataHandling intervalDataHandling) {
		this.intervalDataHandling = intervalDataHandling == null ? IntervalDataHandling
				.find(null) : intervalDataHandling;
	}

	/**
	 * Get the specified {@code IntervalStructureEntry} which is used to
	 * retrieve the start value.
	 * 
	 * @return the specified {@code IntervalStructureEntry} which is used to
	 *         retrieve the start value
	 */
	public IntervalStructureEntry getStartEntry() {
		return startEntry;
	}

	/**
	 * Get the specified {@code IntervalStructureEntry} which is used to
	 * retrieve the end value.
	 * 
	 * @return the specified {@code IntervalStructureEntry} which is used to
	 *         retrieve the end value
	 */
	public IntervalStructureEntry getEndEntry() {
		return endEntry;
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
	public abstract IndexDimensionSlice<?>[] getSlices(
			final Object start, final Object end, final boolean startInclusive,
			final boolean endInclusive);

	/**
	 * Gets the {@code Mapper}.
	 * 
	 * @return the {@code Mapper}
	 */
	protected BaseMapper<?> getMapper() {
		return mapper;
	}

	/**
	 * Gets the created index.
	 * 
	 * @return the created index
	 */
	protected IRangeQueryOptimized getIndex() {
		return index;
	}

	@Override
	public void optimize() {
		for (final IndexDimensionSlice<?> slice : getSlices()) {
			if (slice != null) {
				slice.optimize();
			}
		}
	}

	@Override
	public void save(final BasePersistor persistor)
			throws ForwardedRuntimeException {

		for (final IndexDimensionSlice<?> slice : getSlices()) {
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
		if (getIndex().getObject(id) != null) {
			throw new ForwardedRuntimeException(PersistorException.class, 1004,
					"The identifier '" + id + "' already exists.");
		} else if (id instanceof Number == false) {
			throw new ForwardedRuntimeException(PersistorException.class, 1004,
					"The identifier '" + id + "' is not a number.");
		}

		// create the slice
		final IndexDimensionSlice<?> slice = createSlice((Number) id);

		// load the slice from the InputStream
		try {
			slice.getBitmap().deserialize(new DataInputStream(inputStream));
		} catch (final IOException e) {
			throw new ForwardedRuntimeException(PersistorException.class, 1004,
					e, e.getMessage());
		}

		// add the slice
		getIndex().addObject(slice);
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
	 * Gets the factory used to create indexes, i.e. bitmaps or indexed
	 * collections.
	 * 
	 * @return the factory used to create indexes
	 */
	protected BaseIndexFactory getIndexFactory() {
		return indexFactory;
	}

	/**
	 * Gets the normalized start value of the index.
	 * 
	 * @return the normalized start value of the index
	 */
	public long getNormStart() {
		return getMapper().getNormStartAsLong();
	}

	/**
	 * Gets the normalized end value of the index.
	 * 
	 * @return the normalized end value of the index
	 */
	public long getNormEnd() {
		return getMapper().getNormEndAsLong();
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
	public abstract Object getValue(final Object start,
			final boolean startInclusive, final long pos);

	/**
	 * Determines the value associated to the specified {@code normalizedValue}.
	 * 
	 * @param normalizedValue
	 *            the value to get the associated object for
	 * 
	 * @return the object associated to the {@code normalizedValue}
	 */
	public Object getValue(final long normalizedValue) {
		return getMapper().resolve(normalizedValue);
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
