package net.meisen.dissertation.model.indexes.datarecord;

import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.util.ArrayList;
import java.util.List;
import java.util.UUID;

import net.meisen.dissertation.exceptions.PersistorException;
import net.meisen.dissertation.model.cache.IBitmapIdCache;
import net.meisen.dissertation.model.data.IntervalModel;
import net.meisen.dissertation.model.data.MetaDataModel;
import net.meisen.dissertation.model.data.TidaModel;
import net.meisen.dissertation.model.datasets.IDataRecord;
import net.meisen.dissertation.model.datastructure.IntervalStructureEntry;
import net.meisen.dissertation.model.descriptors.Descriptor;
import net.meisen.dissertation.model.descriptors.DescriptorModel;
import net.meisen.dissertation.model.descriptors.FactDescriptor;
import net.meisen.dissertation.model.indexes.BaseIndexFactory;
import net.meisen.dissertation.model.indexes.IRangeQueryOptimized;
import net.meisen.dissertation.model.indexes.IndexKeyDefinition;
import net.meisen.dissertation.model.indexes.datarecord.slices.Bitmap;
import net.meisen.dissertation.model.indexes.datarecord.slices.BitmapId;
import net.meisen.dissertation.model.indexes.datarecord.slices.FactDescriptorModelSet;
import net.meisen.dissertation.model.indexes.datarecord.slices.SliceId;
import net.meisen.dissertation.model.indexes.datarecord.slices.SliceWithDescriptors;
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
public class IntervalIndex implements IDataRecordIndex {
	private final static Logger LOG = LoggerFactory
			.getLogger(IntervalIndex.class);
	private final static String EXTENSION = ".slice";

	private final MetaDataModel metaDataModel;
	private final IBitmapIdCache<Bitmap> bitmapCache;
	private final IBitmapIdCache<FactDescriptorModelSet> factsCache;
	private final IRangeQueryOptimized index;
	private final BaseMapper<?> mapper;

	private Group persistentGroup = null;

	/**
	 * Constructor to create an index for the specified {@code TidaModel}.
	 * 
	 * @param model
	 *            the {@code TidaModel} to create the index for
	 */
	public IntervalIndex(final TidaModel model) {
		this(model.getIntervalModel(), model.getMetaDataModel(), model
				.getBitmapCache(), model.getFactsCache(), model
				.getIndexFactory());
	}

	/**
	 * Constructor to create an index for the specified {@code IntervalModel}.
	 * 
	 * @param intervalModel
	 *            the {@code IntervalModel} to create the index for
	 * @param metaDataModel
	 *            the {@code MetaDataModel} is needed to lookup descriptors when
	 *            loading an index
	 * @param bitmapCache
	 *            the cache used by the model to cache {@code Bitmap} instances
	 * @param factsCache
	 *            the cache used by the model to cache
	 *            {@code FactDescriptorModelSet} instances
	 * @param indexFactory
	 *            the factory used to create indexes
	 */
	public IntervalIndex(final IntervalModel intervalModel,
			final MetaDataModel metaDataModel,
			final IBitmapIdCache<Bitmap> bitmapCache,
			final IBitmapIdCache<FactDescriptorModelSet> factsCache,
			final BaseIndexFactory indexFactory) {
		this.bitmapCache = bitmapCache;
		this.factsCache = factsCache;
		this.mapper = intervalModel.getTimelineMapper();
		this.metaDataModel = metaDataModel;

		this.index = createIndex(intervalModel, indexFactory);
	}

	/**
	 * Creates a {@code IndexedCollection} for the timeline.
	 * 
	 * @param intervalModel
	 *            the {@code IntervalModel} defining what kind of index to be
	 *            defined
	 * @param indexFactory
	 *            the factory used to create indexes
	 * 
	 * @return the created {@code IndexedCollection}
	 */
	protected IRangeQueryOptimized createIndex(
			final IntervalModel intervalModel,
			final BaseIndexFactory indexFactory) {

		// get the mapper
		final BaseMapper<?> mapper = intervalModel.getTimelineMapper();

		// do some logging
		if (LOG.isTraceEnabled()) {
			LOG.trace("Creating index for '"
					+ mapper.format(mapper.demap(mapper.getStart())) + " - "
					+ mapper.format(mapper.demap(mapper.getEnd()))
					+ "' using granularity '" + mapper.getGranularity()
					+ "'...");
		}

		// create the index
		final IndexKeyDefinition indexKeyDef = new IndexKeyDefinition(
				SliceWithDescriptors.class, "getId");
		indexKeyDef.overrideType(0, mapper.getTargetType());
		final IRangeQueryOptimized index = indexFactory
				.createRangeQueryOptimized(indexKeyDef);

		// the maximum value of the index is defined by the mapper
		index.setMaxValue(mapper.getNormEndAsLong());

		// log the successful creation
		if (LOG.isDebugEnabled()) {
			LOG.debug("Created index for '"
					+ mapper.format(mapper.demap(mapper.getStart())) + " - "
					+ mapper.format(mapper.demap(mapper.getEnd()))
					+ "' with index '" + index.getClass().getName()
					+ "' for identifiers of the intervalIndex of type '"
					+ mapper.getTargetType().getName() + "'.");
		}

		// create the slices from the cache
		for (final BitmapId<?> bitmapId : bitmapCache) {
			if (IntervalIndex.class.equals(bitmapId.getType())) {
				index.addObject(createSlice((Number) bitmapId.getId()));
			}
		}

		return index;
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
	public void index(final ProcessedDataRecord rec) {
		if (rec == null) {
			return;
		} else if (rec.getStart() < 0 || rec.getEnd() < 0) {
			return;
		}

		for (long i = rec.getStart(); i < rec.getEnd() + 1; i++) {
			SliceWithDescriptors<?> slice = (SliceWithDescriptors<?>) index
					.getObject(i);

			// create a new slice
			if (slice == null) {
				slice = createSlice(i);
				index.addObject(slice);
			}

			// set the values of the slice
			slice.set(rec.getId(), rec.getAllDescriptors());
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
	 * @return the slices, which might contain {@code null} values if no data is
	 *         added to the slice yet
	 */
	public SliceWithDescriptors<?>[] getSlices(final long start, final long end) {
		return castSlices(index.getObjectsByStartAndEnd(
				Math.max(mapper.getNormStartAsLong(), start),
				Math.min(mapper.getNormEndAsLong(), end)));
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
	 * @return the slices, which might contain {@code null} values if no data is
	 *         added to the slice yet
	 */
	public SliceWithDescriptors<?>[] getSlicesByTimePoints(final Object start,
			final Object end) {
		return getSlicesByTimePoints(start, end, true, true);
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
	 * Method to create an empty slice with the specified id.
	 * 
	 * @param id
	 *            the identifier of the slice
	 * 
	 * @return the created {@code Slice}
	 */
	protected SliceWithDescriptors<?> createSlice(final Number id) {

		// get the type of the slice
		final Class<? extends Number> clazz = getType();

		// create the slice
		final SliceWithDescriptors<?> slice;
		if (Byte.class.equals(clazz)) {
			final SliceId<Byte> sliceId = new SliceId<Byte>(
					Numbers.castToByte(id), IntervalIndex.class);
			slice = new SliceWithDescriptors<Byte>(sliceId, bitmapCache,
					factsCache);
		} else if (Short.class.equals(clazz)) {
			final SliceId<Short> sliceId = new SliceId<Short>(
					Numbers.castToShort(id), IntervalIndex.class);
			slice = new SliceWithDescriptors<Short>(sliceId, bitmapCache,
					factsCache);
		} else if (Integer.class.equals(clazz)) {
			final SliceId<Integer> sliceId = new SliceId<Integer>(
					Numbers.castToInt(id), IntervalIndex.class);
			slice = new SliceWithDescriptors<Integer>(sliceId, bitmapCache,
					factsCache);
		} else {
			final SliceId<Long> sliceId = new SliceId<Long>(
					Numbers.castToLong(id), IntervalIndex.class);
			slice = new SliceWithDescriptors<Long>(sliceId, bitmapCache,
					factsCache);
		}

		return slice;
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
	public SliceWithDescriptors<?>[] getSlicesByTimePoints(final Object start,
			final Object end, final boolean startInclusive,
			final boolean endInclusive) {
		final long[] bounds = mapper.getBounds(start, end, startInclusive,
				endInclusive);

		return getSlicesByTimePoints(bounds);
	}

	/**
	 * Gets the interval dimensions for the specified {@code start} and
	 * {@code end} values.
	 * 
	 * @param bounds
	 *            the bounds as retrieved by e.g.
	 *            {@link BaseMapper#getBounds(Object, Object, boolean, boolean)}
	 * 
	 * @return the slices between the bounds
	 */
	public SliceWithDescriptors<?>[] getSlicesByTimePoints(final long[] bounds) {
		if (bounds == null) {
			return new SliceWithDescriptors<?>[0];
		} else {
			return getSlices(bounds[0], bounds[1]);
		}
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
				persistor.writeInt(out, slice.numberOfModels());
				for (final String modelId : slice.models()) {
					persistor.writeString(out, modelId);
					persistor.writeInt(out, slice.numberOfFacts(modelId));

					for (final FactDescriptor<?> factDesc : slice
							.facts(modelId)) {

						if (factDesc.isValueInvariant()) {
							persistor.writeByte(out, (byte) 0);
							persistor.writeDouble(out, factDesc.getFact());
						} else {
							final Descriptor<?, ?, ?> desc = metaDataModel
									.getDescriptor(factDesc.getModelId(),
											factDesc.getId());
							persistor.writeByte(out, (byte) 1);
							persistor.writeString(out, desc.getUniqueString());
						}
					}
				}

				slice.getBitmap().serialize(new DataOutputStream(out));
			} catch (final IOException e) {
				throw new ForwardedRuntimeException(PersistorException.class,
						1003, e, e.getMessage());
			} finally {
				persistor.close(identifier);
			}
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

		// get the descriptors associated
		final List<FactDescriptor<?>> descriptors = new ArrayList<FactDescriptor<?>>();
		try {
			final int nrOfModels = persistor.readInt(inputStream);
			for (int i = 0; i < nrOfModels; i++) {
				final String modelId = persistor.readString(inputStream);
				final int nrOfFacts = persistor.readInt(inputStream);

				final DescriptorModel<?> model = metaDataModel
						.getDescriptorModel(modelId);

				for (int k = 0; k < nrOfFacts; k++) {
					final byte flag = persistor.readByte(inputStream);

					if (flag == 0) {
						final double fact = persistor.readDouble(inputStream);

						@SuppressWarnings("rawtypes")
						final FactDescriptor<?> factDesc = new FactDescriptor(
								modelId, fact);
						descriptors.add(factDesc);
					} else {
						final String value = persistor.readString(inputStream);
						final Descriptor<?, ?, ?> desc = model
								.getDescriptorByString(value);
						descriptors.add(desc.getFactDescriptor());
					}
				}
			}
		} catch (final Exception e) {
			throw new ForwardedRuntimeException(PersistorException.class, 1004,
					e, e.getMessage());
		}

		// create the slice
		final SliceWithDescriptors<?> slice = createSlice((Number) id);

		// load the slice from the InputStream
		try {
			slice.deserialize(new DataInputStream(inputStream), descriptors);
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
}
