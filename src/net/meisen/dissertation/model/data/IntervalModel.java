package net.meisen.dissertation.model.data;

import java.util.List;

import net.meisen.dissertation.config.xslt.DefaultValues;
import net.meisen.dissertation.exceptions.IntervalModelException;
import net.meisen.dissertation.impl.indexes.datarecord.intervalindex.ByteIntervalIndex;
import net.meisen.dissertation.impl.indexes.datarecord.intervalindex.IntIntervalIndex;
import net.meisen.dissertation.impl.indexes.datarecord.intervalindex.LongIntervalIndex;
import net.meisen.dissertation.impl.indexes.datarecord.intervalindex.ShortIntervalIndex;
import net.meisen.dissertation.model.datastructure.IntervalStructureEntry;
import net.meisen.dissertation.model.datastructure.IntervalStructureEntry.IntervalTypeFactory.IntervalType;
import net.meisen.dissertation.model.indexes.BaseIndexFactory;
import net.meisen.dissertation.model.indexes.datarecord.BaseIntervalIndex;
import net.meisen.dissertation.model.time.mapper.BaseMapper;
import net.meisen.dissertation.model.time.mapper.BaseMapperFactory;
import net.meisen.dissertation.model.time.timeline.TimelineDefinition;
import net.meisen.general.genmisc.exceptions.registry.IExceptionRegistry;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;

/**
 * The model for the intervals within the Tida-system.
 * 
 * @author pmeisen
 * 
 */
public class IntervalModel {

	@Autowired
	@Qualifier(DefaultValues.EXCEPTIONREGISTRY_ID)
	private IExceptionRegistry exceptionRegistry;

	@Autowired
	@Qualifier(DefaultValues.INDEXFACTORY_ID)
	private BaseIndexFactory indexFactory;

	@Autowired
	@Qualifier(DefaultValues.MAPPERFACTORY_ID)
	private BaseMapperFactory mapperFactory;

	@Autowired
	@Qualifier(DefaultValues.TIMELINEDEFINITION_ID)
	private TimelineDefinition timeline;

	/**
	 * Creates a {@code IntervalModel} which must be completly wired prior to
	 * it's usage to ensure that a {@code indexFactory} is available.
	 */
	public IntervalModel() {
		this(null, null, null);
	}

	/**
	 * Creates a {@code IntervalModel}. The instance must be wired prior to it's
	 * usage to ensure that a {@code indexFactory} is available.
	 * 
	 * @param timeline
	 *            the {@code TimelineDefinition} for the model, can be
	 *            {@code null} if so a default {@code TimelineDefinition} will
	 *            be created and might be reset by auto-wiring.
	 */
	public IntervalModel(final TimelineDefinition timeline) {
		this(timeline, null, null);
	}

	/**
	 * Creates a {@code IntervalModel} with the specified {@code indexFactory},
	 * which should not be {@code null}. If the {@code indexFactory} should be
	 * {@code null} use another constructor and read its information.
	 * 
	 * @param timeline
	 *            the {@code TimelineDefinition} for the model, can be
	 *            {@code null} if so a default {@code TimelineDefinition} will
	 *            be created and might be reset by auto-wiring.
	 * @param indexFactory
	 *            the {@code IndexFactory} used to determine the indexes to be
	 *            used
	 * @param mapperFactory
	 *            the {@code MapperFactory} to be used which is used to create
	 *            the {@code Mapper} instances needed
	 * 
	 * @see BaseIndexFactory
	 * @see BaseMapperFactory
	 */
	public IntervalModel(final TimelineDefinition timeline,
			final BaseIndexFactory indexFactory,
			final BaseMapperFactory mapperFactory) {

		// set the definition of the timeline
		this.timeline = timeline == null ? new TimelineDefinition() : timeline;

		// set the factories
		this.indexFactory = indexFactory;
		this.mapperFactory = mapperFactory;
	}

	/**
	 * Creates a {@code IntervalIndex} for the timeline.
	 * 
	 * @param structure
	 *            the defined {@code DataStructure} with the intervals defined
	 * 
	 * @return the created {@code IntervalIndex}
	 */
	public BaseIntervalIndex createIndex(final DataStructure structure) {

		// make sure needed stuff is known
		if (timeline == null) {
			exceptionRegistry
					.throwException(IntervalModelException.class, 1001);
		}

		// determine the start and end of the dataRecord
		final List<IntervalStructureEntry> entries = structure
				.getEntriesByClass(IntervalStructureEntry.class);

		// search for start and end
		IntervalStructureEntry startEntry = null, endEntry = null;
		for (final IntervalStructureEntry entry : entries) {
			final IntervalType type = entry.getType();

			// determine the start and end of the interval
			if (IntervalType.START.equals(type)) {
				if (startEntry != null) {
					exceptionRegistry.throwException(
							IntervalModelException.class, 1002);
				}
				startEntry = entry;
			} else if (IntervalType.END.equals(type)) {
				if (endEntry != null) {
					exceptionRegistry.throwException(
							IntervalModelException.class, 1003);
				}
				endEntry = entry;
			} else {
				exceptionRegistry.throwException(IntervalModelException.class,
						1004, entry);
			}
		}

		// create the mapper
		final BaseMapper<?> mapper = createMapper(timeline.getStart(),
				timeline.getEnd());

		// create the index
		final BaseIntervalIndex index;

		// create the IntervalIndex depending on the mapper
		if (Byte.class.equals(mapper.getTargetType())) {
			index = new ByteIntervalIndex(mapper, startEntry, endEntry,
					getIndexFactory());
		} else if (Short.class.equals(mapper.getTargetType())) {
			index = new ShortIntervalIndex(mapper, startEntry, endEntry,
					getIndexFactory());
		} else if (Integer.class.equals(mapper.getTargetType())) {
			index = new IntIntervalIndex(mapper, startEntry, endEntry,
					getIndexFactory());
		} else {
			index = new LongIntervalIndex(mapper, startEntry, endEntry,
					getIndexFactory());
		}

		return index;
	}

	/**
	 * Gets the {@code indexFactory} specified for the {@code IntervalModel}.
	 * This method should never return {@code null} if the {@code IntervalModel}
	 * is assumed to be initialized.
	 * 
	 * @return the {@code indexFactory} specified for the {@code IntervalModel}
	 */
	public BaseIndexFactory getIndexFactory() {
		return indexFactory;
	}

	/**
	 * Gets the {@code MapperFactory} used to create mappers which map values of
	 * the interval-model to it's internal representation.
	 * 
	 * @return the {@code MapperFactory} used to used to create mappers which
	 *         map values of the interval-model to it's internal representation
	 * 
	 * @see BaseMapperFactory
	 */
	public BaseMapperFactory getMapperFactory() {
		return mapperFactory;
	}

	/**
	 * Gets the {@code Mapper} to be used for the specified {@code start} and
	 * {@code end}.
	 * 
	 * @param start
	 *            the start value of the mapper
	 * @param end
	 *            the end value of the mapper
	 * 
	 * @return the {@code Mapper to be used}
	 * 
	 * @see BaseMapper
	 */
	protected BaseMapper<?> createMapper(final Object start, final Object end) {
		final BaseMapperFactory factory = getMapperFactory();
		if (factory == null) {
			exceptionRegistry
					.throwException(IntervalModelException.class, 1000);
			return null;
		} else if (timeline == null) {
			exceptionRegistry
					.throwException(IntervalModelException.class, 1001);
			return null;
		} else {
			return factory.createWithObjects(start, end,
					timeline.getGranularity());
		}
	}

	/**
	 * Gets the {@code TimelineDefinition} used by {@code this} instance.
	 * 
	 * @return the {@code TimelineDefinition}
	 */
	public TimelineDefinition getTimelineDefinition() {
		return timeline;
	}
}
