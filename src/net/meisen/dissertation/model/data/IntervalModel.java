package net.meisen.dissertation.model.data;

import net.meisen.dissertation.config.xslt.DefaultValues;
import net.meisen.dissertation.exceptions.IntervalModelException;
import net.meisen.dissertation.model.indexes.BaseIndexFactory;
import net.meisen.dissertation.model.indexes.datarecord.IntervalDataHandling;
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

	/**
	 * A result of a mapping process, i.e. if an interval {@code [start, end]}
	 * is mapped to a specific
	 * 
	 * @author pmeisen
	 * 
	 */
	public class MappingResult {
		private final long start;
		private final long end;

		/**
		 * Constructor specifying the mapped results.
		 * 
		 * @param start
		 *            the mapped start result
		 * @param end
		 *            the mapped end result
		 */
		public MappingResult(final long start, final long end) {
			this.start = start;
			this.end = end;
		}

		/**
		 * Gets the mapped start value.
		 * 
		 * @return the mapped start value
		 */
		public long getStart() {
			return start;
		}

		/**
		 * Gets the mapped end value.
		 * 
		 * @return the mapped end value
		 */
		public long getEnd() {
			return end;
		}
	}

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

	private BaseMapper<?> timelineMapper = null;

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
	 * Creates a mapper useful for the complete defined timeline.
	 * 
	 * @return the {@code Mapper} to be used
	 */
	public BaseMapper<?> createMapper() {
		return createMapper(timeline.getStart(), timeline.getEnd());
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
	 * @return the {@code Mapper} to be used
	 * 
	 * @see BaseMapper
	 */
	public BaseMapper<?> createMapper(final Object start, final Object end) {
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

	/**
	 * Gets the mapper used to map values to the defined {@code Timeline}.
	 * 
	 * @return the mapper used to map values to the defined {@code Timeline}
	 * 
	 * @see TimelineDefinition
	 */
	public BaseMapper<?> getTimelineMapper() {
		if (timelineMapper == null) {
			timelineMapper = createMapper();
		}

		return timelineMapper;
	}

	/**
	 * Maps the {@code [start, end]} interval to the values defined by the
	 * timeline.
	 * 
	 * @param start
	 *            the start value to be mapped
	 * @param end
	 *            the end value to be mapped
	 * @param handling
	 *            the {@code IntervalDataHandling} used to map the values
	 * 
	 * @return the result of the mapping
	 * 
	 * @throws IntervalModelException
	 *             if a {@code null} value is specified for {@code start} and/or
	 *             {@code end} and the defined {@code handling} is
	 *             {@link IntervalDataHandling#FAILONNULL}
	 */
	public MappingResult mapToTimeline(final Object start, final Object end,
			final IntervalDataHandling handling) {
		return mapToTimeline(start, end, handling, true, true);
	}

	/**
	 * Depending on {@code startInclusive} and {@code endInclusive} the method
	 * maps the {@code [start, end]} interval, {@code (start, end)} interval,
	 * {@code (start, end]} interval, or {@code [start, end)} interval to the
	 * values defined by the timeline.
	 * 
	 * @param start
	 *            the start value to be mapped
	 * @param end
	 *            the end value to be mapped
	 * @param handling
	 *            the {@code IntervalDataHandling} used to map the values
	 * @param startInclusive
	 *            {@code true} to define the start value to be inclusive,
	 *            otherwise it is exclusive
	 * @param endInclusive
	 *            {@code true} to define the start value to be inclusive,
	 *            otherwise it is exclusive
	 * 
	 * @return the result of the mapping
	 * 
	 * @throws IntervalModelException
	 *             if a {@code null} value is specified for {@code start} and/or
	 *             {@code end} and the defined {@code handling} is
	 *             {@link IntervalDataHandling#FAILONNULL}
	 */
	public MappingResult mapToTimeline(final Object start, final Object end,
			final IntervalDataHandling handling, final boolean startInclusive,
			final boolean endInclusive) throws IntervalModelException {
		final BaseMapper<?> mapper = getTimelineMapper();

		// check for nulls
		final boolean startIsNull = start == null;
		final boolean endIsNull = end == null;

		// get the norm values
		long normStart, normEnd;
		if (startIsNull || endIsNull) {

			if (IntervalDataHandling.FAILONNULL.equals(handling)) {
				exceptionRegistry.throwException(IntervalModelException.class,
						1002, getTimelineMapper().format(start),
						mapper.format(end));
				return null;
			} else if (IntervalDataHandling.USEOTHER.equals(handling)) {
				if (startIsNull && !endIsNull) {
					normStart = endInclusive ? mapper.mapToLong(end) : mapper
							.shiftToLong(end, 1, true);
					normEnd = normStart;
				} else if (startIsNull && endIsNull) {
					normStart = -1;
					normEnd = -1;
				} else if (!startIsNull && endIsNull) {
					normStart = startInclusive ? mapper.mapToLong(start)
							: mapper.shiftToLong(start, 1, false);
					normEnd = normStart;
				} else {
					throw new IllegalStateException(
							"This state cannot be reached (start: " + start
									+ ", end: " + end + ")!");
				}
			} else if (IntervalDataHandling.BOUNDARIESWHENNULL.equals(handling)) {
				if (startIsNull) {
					normStart = mapper.getNormStartAsLong();
				} else {
					normStart = startInclusive ? mapper.mapToLong(start)
							: mapper.shiftToLong(start, 1, false);
				}
				if (endIsNull) {
					normEnd = mapper.getNormEndAsLong();
				} else {
					normEnd = endInclusive ? mapper.mapToLong(end) : mapper
							.shiftToLong(end, 1, true);
				}
			} else {
				throw new UnsupportedOperationException(
						"The intervalHandling '" + handling
								+ "' is not supported.");
			}
		} else {
			normStart = startInclusive ? mapper.mapToLong(start) : mapper
					.shiftToLong(start, 1, false);
			normEnd = endInclusive ? mapper.mapToLong(end) : mapper
					.shiftToLong(end, 1, true);
		}

		// if one value is invalid both are
		if (normStart == -1 || normEnd == -1) {
			normStart = -1;
			normEnd = -1;
		}
		// check if the interval is valid at all
		else if (normStart > normEnd) {
			normStart = -1;
			normEnd = -1;
		}
		// check if a value undercut or exceeded the timeline
		else if (normStart == normEnd) {
			if (normStart == mapper.getNormEndAsLong()) {

				/*
				 * If the start wasn't null, check if the start exceeded the
				 * end. If it exceeds the end, it's an invalid interval
				 * considering the timeline.
				 */
				if (!startIsNull && mapper.isLargerThanEnd(start)) {
					normStart = -1;
					normEnd = -1;
				}
				/*
				 * If the start is null, check if the end was a valid value. If
				 * not the whole interval is invalid.
				 */
				else if (startIsNull
						&& (mapper.isLargerThanEnd(end) || mapper
								.isSmallerThanStart(end))) {
					normStart = -1;
					normEnd = -1;
				}
			} else if (normEnd == mapper.getNormStartAsLong()) {

				/*
				 * If the end wasn't null, check if the value was valid
				 * considering the start.
				 */
				if (!endIsNull && mapper.isSmallerThanStart(end)) {
					normStart = -1;
					normEnd = -1;
				}
				/*
				 * If the end is null, check if the start was a valid value. If
				 * not the whole interval is invalid.
				 */
				else if (endIsNull
						&& (mapper.isLargerThanEnd(start) || mapper
								.isSmallerThanStart(start))) {
					normStart = -1;
					normEnd = -1;
				}
			}
		}

		return new MappingResult(normStart, normEnd);
	}
}
