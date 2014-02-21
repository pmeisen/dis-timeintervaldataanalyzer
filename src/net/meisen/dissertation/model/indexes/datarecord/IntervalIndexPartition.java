package net.meisen.dissertation.model.indexes.datarecord;

import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.util.Date;
import java.util.UUID;

import net.meisen.dissertation.exceptions.PersistorException;
import net.meisen.dissertation.model.datasets.IDataRecord;
import net.meisen.dissertation.model.datastructure.IntervalStructureEntry;
import net.meisen.dissertation.model.indexes.BaseIndexedCollectionFactory;
import net.meisen.dissertation.model.indexes.IRangeQueryOptimized;
import net.meisen.dissertation.model.indexes.IndexKeyDefinition;
import net.meisen.dissertation.model.indexes.datarecord.slices.IndexDimensionSlice;
import net.meisen.dissertation.model.persistence.BasePersistor;
import net.meisen.dissertation.model.persistence.Group;
import net.meisen.dissertation.model.persistence.Identifier;
import net.meisen.dissertation.model.time.mapper.BaseMapper;
import net.meisen.general.genmisc.exceptions.ForwardedRuntimeException;
import net.meisen.general.genmisc.types.Dates;
import net.meisen.general.genmisc.types.Numbers;
import net.meisen.general.genmisc.types.Objects;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * An {@code IntervalIndexPartition} is normally defined as a partition (or the
 * whole) timeline. The size of the partition is defined by a {@code Mapper},
 * which is used to map the data-values to the underlying part of the timeline.
 * 
 * @author pmeisen
 * 
 */
public abstract class IntervalIndexPartition implements DataRecordIndex {
	private final static Logger LOG = LoggerFactory
			.getLogger(IntervalIndexPartition.class);
	private final static String EXTENSION = ".slice";

	@SuppressWarnings("rawtypes")
	private final BaseMapper mapper;

	private final IntervalStructureEntry startEntry;
	private final IntervalStructureEntry endEntry;
	private final IRangeQueryOptimized index;

	private IntervalDataHandling intervalDataHandling;
	private Group persistentGroup = null;

	/**
	 * Constructor to create a partition using the specified {@code Mapper}, the
	 * {@code start}- and {@code end}-entry and the specified
	 * {@code indexedCollectionFactory} to create the needed indexes.
	 * 
	 * @param mapper
	 *            the {@code Mapper} which defines the start and end value, as
	 *            well as the type of the indexed values
	 * @param start
	 *            the {@code entry} which defines the start
	 * @param end
	 *            the {@code entry} which defines the end
	 * @param indexedCollectionFactory
	 *            the {@code indexedCollectionFactory} to create the needed
	 *            indexes
	 */
	public IntervalIndexPartition(final BaseMapper<?> mapper,
			final IntervalStructureEntry start,
			final IntervalStructureEntry end,
			final BaseIndexedCollectionFactory indexedCollectionFactory) {
		if (LOG.isTraceEnabled()) {
			LOG.trace("Creating " + getClass().getSimpleName() + " for '"
					+ mapper.demap(mapper.getStart()) + " - "
					+ mapper.demap(mapper.getEnd()) + "'...");
		}

		// set the entries
		this.startEntry = start;
		this.endEntry = end;

		// set the mapper
		this.mapper = mapper;

		// create an index to handle the different values for a descriptor
		final IndexKeyDefinition indexKeyDef = new IndexKeyDefinition(
				IndexDimensionSlice.class, "getId");
		indexKeyDef.overrideType(0, getType());
		this.index = indexedCollectionFactory
				.createRangeQueryOptimized(indexKeyDef);

		// the maximum value of the index is defined by the mapper
		this.index.setMaxValue(this.mapper.getNormEndAsLong());

		// set the default intervalDataHandling
		setIntervalDataHandling(null);

		// log the successful creation
		if (LOG.isTraceEnabled()) {
			LOG.trace("Created " + getClass().getSimpleName() + " for '"
					+ mapper.demap(mapper.getStart()) + " - "
					+ mapper.demap(mapper.getEnd()) + "' with index '"
					+ this.index.getClass().getName()
					+ "' for identifiers of the partition of type '"
					+ getType().getName() + "'.");
		}
	}

	/**
	 * Gets a unique identifier for the partition on the timeline (if the
	 * partitions are disjunct).
	 * 
	 * @return a unique identifier for the partition
	 */
	public long getId() {
		return mapper.getStart();
	}

	/**
	 * Gets a readable version of the partition's identifier.
	 * 
	 * @return a readable version of the partition's identifier
	 */
	public String getPartitionId() {
		return getFormattedId(getMapper().getNormStartAsLong());
	}

	/**
	 * Formats an id of a slice of the interval's partition nicely.
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
			return "Invalid '" + id + "'";
		}
	}

	/**
	 * Formats an id of a slice of the interval's partition nicely.
	 * 
	 * @param id
	 *            the identifier of a slice to be formatted
	 * 
	 * @return the formatted identifier
	 */
	public String getFormattedId(final long id) {
		final Object denStart = getMapper().resolve(id);
		if (Date.class.isAssignableFrom(getType())) {
			return Dates.formatDate((Date) denStart, "dd.MM.yyyy HH:mm:ss,SSS");
		} else {
			return denStart.toString();
		}
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
	 * Gets the amounts of slices, i.e. different values within the partition.
	 * 
	 * @return the amounts of slices
	 */
	public int getAmountOfSlices() {
		return index.size();
	}

	/**
	 * Gets the slices of the {@code IntervalIndexPartition}.
	 * 
	 * @return the slices of the {@code IntervalIndexPartition}
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
			final String fileName = UUID.randomUUID().toString() + EXTENSION;
			final Identifier id = new Identifier(fileName, persistentGroup);
			id.setComment(mapper.demap(
					Numbers.castToLong((Number) slice.getId())).toString());
			final OutputStream out = persistor.openForWrite(id);

			try {
				persistor.writeObject(out, slice.getId());
				slice.getBitmap().serialize(new DataOutputStream(out));
			} catch (final IOException e) {
				throw new ForwardedRuntimeException(PersistorException.class,
						1003, e, e.getMessage());
			}

			persistor.close(id);
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
}
