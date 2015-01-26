package net.meisen.dissertation.model.dimensions.templates;

import java.util.Date;
import java.util.Iterator;
import java.util.TimeZone;

import net.meisen.dissertation.exceptions.TimeDimensionException;
import net.meisen.dissertation.model.data.IntervalModel;
import net.meisen.dissertation.model.dimensions.TimeLevelMember;
import net.meisen.dissertation.model.dimensions.TimeMemberRange;
import net.meisen.dissertation.model.time.granularity.DateFormat;
import net.meisen.dissertation.model.time.granularity.IDateBasedGranularity;
import net.meisen.dissertation.model.time.granularity.ITimeGranularity;
import net.meisen.dissertation.model.time.granularity.ITimeGranularityFactory;
import net.meisen.dissertation.model.time.mapper.BaseMapper;
import net.meisen.general.genmisc.exceptions.ForwardedRuntimeException;
import net.meisen.general.genmisc.types.Dates;
import net.meisen.general.genmisc.types.Strings;

/**
 * A template for a minute-level.
 * 
 * @author pmeisen
 * 
 */
public class Rasters extends BaseTimeLevelTemplate implements
		ITimeLevelTemplateFactory {
	private final int bucketSize;
	private final IDateBasedGranularity levelGranularity;
	private final IDateBasedGranularity groupGranularity;

	private static class Formatter {

		private final BaseMapper<?> mapper;
		private final boolean addLevelDSTMarker;
		private final boolean addGroupDSTMarker;

		private final String shortIdFormat;
		private final String shortNameFormat;
		private final String groupIdFormat;
		private final String groupNameFormat;

		public Formatter() {
			mapper = null;
			shortIdFormat = null;
			shortNameFormat = null;
			groupIdFormat = null;
			groupNameFormat = null;

			addGroupDSTMarker = false;
			addLevelDSTMarker = false;
		}

		public Formatter(final BaseMapper<?> mapper,
				final IDateBasedGranularity levelGranularity,
				final IDateBasedGranularity groupGranularity) {
			final DateFormat format = levelGranularity.getFormat();
			final DateFormat groupFormat = groupGranularity.getFormat();

			this.mapper = mapper;
			this.addLevelDSTMarker = levelGranularity.isAssignableTo('h');
			this.addGroupDSTMarker = groupGranularity.isAssignableTo('h');
			this.groupIdFormat = groupFormat.getIdFormat();
			this.groupNameFormat = groupFormat.getPrintFormat();

			// remove leading "_" and " "
			this.shortIdFormat = Strings
					.trim(format.getIdFormat().replace(this.groupIdFormat, "")
							.trim(), "_", "_");

			// remove any " "
			this.shortNameFormat = format.getPrintFormat()
					.replace(this.groupNameFormat, "").trim();
		}

		protected String createId(final long start, final long end,
				final String timezone) {

			if (shortIdFormat.length() > 0) {
				final Date groupDate = (Date) mapper.resolve(start);
				final String groupId = Dates.formatDate(groupDate,
						groupIdFormat, timezone);

				// get the start and the end
				final Date startDate = (Date) mapper.resolve(start);
				String startId = Dates.formatDate(startDate, shortIdFormat,
						timezone);
				startId = appendDst(addLevelDSTMarker, startId, startDate,
						timezone, false);
				final Date endDate = (Date) mapper.resolve(end);
				String endId = Dates.formatDate(endDate, shortIdFormat,
						timezone);
				endId = appendDst(addLevelDSTMarker, endId, endDate, timezone,
						false);

				return "R" + groupId + "_" + startId + "_" + endId;
			} else {
				final Date groupStartDate = (Date) mapper.resolve(start);
				String groupStart = Dates.formatDate(groupStartDate,
						groupIdFormat, timezone);
				groupStart = appendDst(addGroupDSTMarker, groupStart,
						groupStartDate, timezone, false);
				final Date groupEndDate = (Date) mapper.resolve(end);
				String groupEnd = Dates.formatDate(groupEndDate, groupIdFormat,
						timezone);
				groupEnd = appendDst(addGroupDSTMarker, groupEnd, groupEndDate,
						timezone, false);

				return "R" + groupStart + "_" + groupEnd;
			}
		}

		protected String createName(final long start, final long end,
				final String timezone) {
			final String groupName = Dates.formatDate(
					(Date) mapper.resolve(start), groupNameFormat, timezone);

			if (shortNameFormat.length() > 0) {
				final Date startDate = (Date) mapper.resolve(start);
				String startName = Dates.formatDate(startDate, shortNameFormat,
						timezone);
				startName = appendDst(addLevelDSTMarker, startName, startDate,
						timezone, true);
				final Date endDate = (Date) mapper.resolve(end);
				String endName = Dates.formatDate(endDate, shortNameFormat,
						timezone);
				endName = appendDst(addLevelDSTMarker, endName, endDate,
						timezone, true);

				return "[" + startName + ", " + endName + "] (" + groupName
						+ ")";
			} else {
				final Date groupStartDate = (Date) mapper.resolve(start);
				String groupStart = Dates.formatDate(groupStartDate,
						groupNameFormat, timezone);
				groupStart = appendDst(addGroupDSTMarker, groupStart,
						groupStartDate, timezone, true);
				final Date groupEndDate = (Date) mapper.resolve(end);
				String groupEnd = Dates.formatDate(groupEndDate,
						groupNameFormat, timezone);
				groupEnd = appendDst(addGroupDSTMarker, groupEnd, groupEndDate,
						timezone, true);

				if (groupStart.equals(groupEnd)) {
					return "[" + groupStart + "]";
				} else {
					return "[" + groupStart + ", " + groupEnd + "]";
				}
			}
		}

		protected String appendDst(final boolean add, final String value,
				final Date date, final String timezone, final boolean name) {

			if (add && TimeZone.getTimeZone(timezone).inDaylightTime(date)) {
				return value
						+ (name ? " (" + DST_MARKER + ")" : "_" + DST_MARKER);
			} else {
				return value;
			}
		}

		protected String createId(final long start, final long end) {
			return "R" + start + "-" + end;
		}

		protected String createName(final long start, final long end) {
			return "[" + start + ", " + end + "]";
		}
	}

	/**
	 * Constructor to create the {@code ITimeLevelTemplateFactory} instance.
	 */
	public Rasters() {
		this.bucketSize = -1;
		this.levelGranularity = null;
		this.groupGranularity = null;
	}

	/**
	 * Default constructor to create a raster with the specified
	 * {@code bucketSize}.
	 * 
	 * @param bucketSize
	 *            the size of the buckets of the raster
	 */
	protected Rasters(final int bucketSize) {
		this(bucketSize, null, null);
	}

	/**
	 * A raster for a {@code DateBasedGranularity}, which might be for a
	 * specific level and grouped by a specified group.
	 * 
	 * @param bucketSize
	 *            the size of the buckets
	 * @param levelGranularity
	 *            the granularity of the level (must be smaller than or equal to
	 *            the grouping granularity), can be {@code null}
	 * @param groupGranularity
	 *            the granularity of the group
	 * 
	 * @see IDateBasedGranularity
	 */
	protected Rasters(final int bucketSize,
			final IDateBasedGranularity levelGranularity,
			final IDateBasedGranularity groupGranularity) {
		this.bucketSize = bucketSize;
		this.levelGranularity = levelGranularity;
		this.groupGranularity = groupGranularity;
	}

	/**
	 * Gets the bucket-size of the raster.
	 * 
	 * @return the bucket-size of the raster
	 */
	public int getBucketSize() {
		return bucketSize;
	}

	/**
	 * Gets the granularity used for the group.
	 * 
	 * @return the granularity used for the group
	 */
	public IDateBasedGranularity getGroupGranularity() {
		return groupGranularity;
	}

	/**
	 * Gets the granularity used for the level.
	 * 
	 * @return the granularity used for the level
	 */
	public IDateBasedGranularity getLevelGranularity() {
		return levelGranularity;
	}

	@Override
	public String getId() {
		final String sep = ITimeLevelTemplateFactory.PREFIX_SEPARATOR;

		final String group = (groupGranularity == null ? "" : sep
				+ groupGranularity.getClass().getSimpleName().toUpperCase());
		final String level = (levelGranularity == null ? "" : sep
				+ levelGranularity.getClass().getSimpleName().toUpperCase());
		return getPrefixId() + group + level + sep + bucketSize;
	}

	@Override
	public String getPrefixId() {
		return "RASTER";
	}

	/**
	 * Validates the specified model to be used with {@code this}.
	 * 
	 * @param model
	 *            the model to be validated
	 * @throws ForwardedRuntimeException
	 *             if the model is invalid
	 */
	protected void validate(final IntervalModel model)
			throws ForwardedRuntimeException {
		if (model == null) {
			throw new ForwardedRuntimeException(TimeDimensionException.class,
					1002);
		}
	}

	@Override
	public Iterator<TimeLevelMember> it(final IntervalModel model,
			final String timezone) throws ForwardedRuntimeException {
		validate(model);

		final BaseMapper<?> mapper = model.getTimelineMapper();
		final long s = mapper.getNormStartAsLong();
		final long e = mapper.getNormEndAsLong();
		return it(model, s, e, timezone);
	}

	@Override
	public Iterator<TimeLevelMember> it(final IntervalModel model,
			final long start, final long end, final String timezone)
			throws ForwardedRuntimeException {
		validate(model);

		final ITimeGranularity granularity = model.getTimelineDefinition()
				.getGranularity();

		if (groupGranularity == null) {
			return createSimpleIt(model, start, end);
		} else {

			// make sure we have a date-based granularity
			if (granularity instanceof IDateBasedGranularity == false) {
				throw new ForwardedRuntimeException(
						TimeDimensionException.class, 1004, granularity
								.getClass().getSimpleName());
			}
			final IDateBasedGranularity dateGran = (IDateBasedGranularity) granularity;
			if (!dateGran.isAssignableTo(groupGranularity.getIdentifier())) {
				throw new ForwardedRuntimeException(
						TimeDimensionException.class, 1003, groupGranularity
								.getClass().getSimpleName(), dateGran
								.getClass().getSimpleName());
			}

			return createGroupedIt(model, timezone, start, end);
		}
	}

	/**
	 * Create a simple iterating by the numbers of the time-axis.
	 * 
	 * @param model
	 *            the {@code IntervalModel} used
	 * @param s
	 *            the start value
	 * @param e
	 *            the end value
	 * 
	 * @return a simple iterator
	 */
	protected Iterator<TimeLevelMember> createSimpleIt(
			final IntervalModel model, final long s, final long e) {
		final Formatter formatter = new Formatter();

		return new Iterator<TimeLevelMember>() {
			private long current = s;

			@Override
			public boolean hasNext() {
				return current <= e;
			}

			@Override
			public TimeLevelMember next() {
				final long end = Math.min(e, current + bucketSize - 1);
				final String id = formatter.createId(current, end);
				final TimeLevelMember member = new TimeLevelMember(id, current,
						end);
				member.setName(formatter.createName(current, end));

				current += bucketSize;

				return member;
			}

			@Override
			public void remove() {
				throw new IllegalStateException("Unsupported by this iterator.");
			}
		};
	}

	/**
	 * Create an iterator used to iterate over the the different members.
	 * 
	 * @param model
	 *            the {@code IntervalModel} used
	 * @param timezone
	 *            the time-zone the iterator should use
	 * @param s
	 *            the start value
	 * @param e
	 *            the end value
	 * 
	 * @return the created iterator
	 */
	protected Iterator<TimeLevelMember> createGroupedIt(
			final IntervalModel model, final String timezone, final long s,
			final long e) {
		final ITimeGranularity modelGranularity = model.getTimelineDefinition()
				.getGranularity();

		// get the level's granularity to be used
		final IDateBasedGranularity granularity;
		if (this.levelGranularity == null) {
			if (modelGranularity instanceof IDateBasedGranularity) {
				granularity = (IDateBasedGranularity) modelGranularity;
			} else {
				throw new ForwardedRuntimeException(
						TimeDimensionException.class, 1006, modelGranularity
								.getClass().getSimpleName());
			}
		} else {
			granularity = this.levelGranularity;
		}

		// validate the level granularity
		if (!granularity.isAssignableTo(groupGranularity.getIdentifier())) {
			throw new ForwardedRuntimeException(TimeDimensionException.class,
					1007, granularity.getClass().getSimpleName(),
					groupGranularity.getClass().getSimpleName());
		}

		// create a formatter
		final Formatter formatter = new Formatter(model.getTimelineMapper(),
				granularity, groupGranularity);

		// create the iterator on group-level
		final Iterator<TimeLevelMember> groupIt = createIterator(model,
				timezone, groupGranularity, s, e);

		if (granularity.equals(modelGranularity)) {
			return createLowestGranularityGroupedIt(groupIt, formatter,
					timezone);
		} else if (granularity.equals(groupGranularity)) {
			return createGroupBasedRasterIt(groupIt, formatter, timezone);
		} else {
			return createLevelBasedGroupedIt(model, granularity, groupIt,
					formatter, timezone);
		}
	}

	/**
	 * Creates a group-based iterator, i.e. no level is used in between.
	 * 
	 * @param groupIt
	 *            the group iterator used to iterate over the group
	 * @param formatter
	 *            the formatter for identifier and name
	 * @param timezone
	 *            the time-zone
	 * 
	 * @return the created iterator
	 */
	protected Iterator<TimeLevelMember> createGroupBasedRasterIt(
			final Iterator<TimeLevelMember> groupIt, final Formatter formatter,
			final String timezone) {

		return new Iterator<TimeLevelMember>() {

			@Override
			public boolean hasNext() {
				return groupIt.hasNext();
			}

			@Override
			public TimeLevelMember next() {
				TimeMemberRange range = getSingleRange(groupIt.next());
				final long start = range.getStart();

				long current = 1;
				while (groupIt.hasNext() && current < bucketSize) {
					range = getSingleRange(groupIt.next());
					current++;
				}
				final long end = range.getEnd();
				return createMember(start, end, formatter, timezone);
			}

			@Override
			public void remove() {
				throw new IllegalStateException("Unsupported by this iterator.");
			}
		};
	}

	/**
	 * Creates a level-based iterator, i.e. no level is used in between.
	 * 
	 * @param model
	 *            the {@code IntervalModel} used
	 * @param levelGranularity
	 *            the level granularity, i.e. the bucket-size is applied on that
	 *            level
	 * @param groupIt
	 *            the group iterator used to iterate over the group
	 * @param formatter
	 *            the formatter for identifier and name
	 * @param timezone
	 *            the time-zone
	 * 
	 * @return the created iterator
	 */
	protected Iterator<TimeLevelMember> createLevelBasedGroupedIt(
			final IntervalModel model,
			final IDateBasedGranularity levelGranularity,
			final Iterator<TimeLevelMember> groupIt, final Formatter formatter,
			final String timezone) {

		return new Iterator<TimeLevelMember>() {
			private Iterator<TimeLevelMember> currentLevelIt = null;

			@Override
			public boolean hasNext() {
				return groupIt.hasNext()
						|| (currentLevelIt != null && currentLevelIt.hasNext());
			}

			@Override
			public TimeLevelMember next() {

				// get the next iterator for the level, if needed
				if (currentLevelIt == null || !currentLevelIt.hasNext()) {
					final TimeMemberRange range = getSingleRange(groupIt.next());
					currentLevelIt = createIterator(model, timezone,
							levelGranularity, range.getStart(), range.getEnd());
				}

				// get the next range from the level, if needed
				TimeMemberRange range = getSingleRange(currentLevelIt.next());
				final long start = range.getStart();

				long current = 1;
				while (currentLevelIt.hasNext() && current < bucketSize) {
					range = getSingleRange(currentLevelIt.next());
					current++;
				}
				final long end = range.getEnd();

				return createMember(start, end, formatter, timezone);
			}

			@Override
			public void remove() {
				throw new IllegalStateException("Unsupported by this iterator.");
			}

		};
	}

	/**
	 * Creates an iterator for the lowest granularity.
	 * 
	 * @param groupIt
	 *            the group iterator used to iterate over the group
	 * @param formatter
	 *            the formatter for identifier and name
	 * @param timezone
	 *            the time-zone
	 * 
	 * @return the created iterator
	 */
	protected Iterator<TimeLevelMember> createLowestGranularityGroupedIt(
			final Iterator<TimeLevelMember> groupIt, final Formatter formatter,
			final String timezone) {

		return new Iterator<TimeLevelMember>() {
			private TimeMemberRange currentRange = null;
			private long current = -1;

			@Override
			public boolean hasNext() {
				return groupIt.hasNext()
						|| (currentRange != null && current < currentRange
								.getEnd());
			}

			@Override
			public TimeLevelMember next() {

				// get the next range
				if (currentRange == null || current > currentRange.getEnd()) {

					// get the range
					currentRange = getSingleRange(groupIt.next());
					current = currentRange.getStart();
				}

				final TimeLevelMember member = createMember(currentRange,
						current, formatter, timezone);
				current = member.getRange(0).getEnd() + 1;

				return member;
			}

			@Override
			public void remove() {
				throw new IllegalStateException("Unsupported by this iterator.");
			}
		};
	}

	/**
	 * Creates a {@code TimeLevelMember} for the specified {@code range}.
	 * 
	 * @param range
	 *            the range
	 * @param start
	 *            the start of the member
	 * @param formatter
	 *            the formatter
	 * @param timezone
	 *            the time-zone
	 * 
	 * @return the created member
	 */
	protected TimeLevelMember createMember(final TimeMemberRange range,
			final long start, final Formatter formatter, final String timezone) {

		final long end = Math.min(range.getEnd(), start + bucketSize - 1);
		final String id = formatter.createId(start, end, timezone);
		final TimeLevelMember member = new TimeLevelMember(id, start, end);
		member.setName(formatter.createName(start, end, timezone));

		return member;
	}

	/**
	 * Creates a {@code TimeLevelMember} for the specified {@code range}.
	 * 
	 * 
	 * @param start
	 *            the start of the member
	 * @param end
	 *            the end of the member
	 * @param formatter
	 *            the formatter
	 * @param timezone
	 *            the time-zone
	 * 
	 * @return the created member
	 */
	protected TimeLevelMember createMember(final long start, final long end,
			final Formatter formatter, final String timezone) {
		final String id = formatter.createId(start, end, timezone);
		final String name = formatter.createName(start, end, timezone);
		final TimeLevelMember member = new TimeLevelMember(id, start, end);
		member.setName(name);

		return member;
	}

	/**
	 * Gets the single range of the specified member.
	 * 
	 * @param member
	 *            the member
	 * 
	 * @return the single range
	 * 
	 * @throws ForwardedRuntimeException
	 *             if there is no single range
	 */
	protected TimeMemberRange getSingleRange(final TimeLevelMember member)
			throws ForwardedRuntimeException {

		// make sure that there is only one range
		if (member.sizeOfRanges() != 1) {
			throw new ForwardedRuntimeException(TimeDimensionException.class,
					1005, groupGranularity.getClass().getSimpleName());
		}

		// get the range
		return member.getRange(0);
	}

	@Override
	public boolean isCreator(final String id) {
		return id != null
				&& id.startsWith(getPrefixId()
						+ ITimeLevelTemplateFactory.PREFIX_SEPARATOR);
	}

	@Override
	public Rasters createTemplate(final ITimeGranularityFactory factory,
			final String id) throws ForwardedRuntimeException {
		final String[] parts = id
				.split(ITimeLevelTemplateFactory.PREFIX_SEPARATOR);
		final int length = parts.length;
		if (length < 2) {
			throw new ForwardedRuntimeException(TimeDimensionException.class,
					1008, id);
		}

		// check the bucketSize
		int bucketSize = -1;
		if (length > 1) {
			try {
				bucketSize = Integer.parseInt(parts[length - 1]);
			} catch (final NumberFormatException e) {
				throw new ForwardedRuntimeException(
						TimeDimensionException.class, 1008, id);
			}
		}

		// check the group
		IDateBasedGranularity group = null;
		if (length > 2) {
			final ITimeGranularity gran = factory.find(parts[1]);
			if (gran instanceof IDateBasedGranularity == false) {
				throw new ForwardedRuntimeException(
						TimeDimensionException.class, 1008, id);
			}

			group = (IDateBasedGranularity) gran;
		}

		// check the level
		IDateBasedGranularity level = null;
		if (length > 3) {
			final ITimeGranularity gran = factory.find(parts[2]);
			if (gran instanceof IDateBasedGranularity == false) {
				throw new ForwardedRuntimeException(
						TimeDimensionException.class, 1008, id);
			}

			level = (IDateBasedGranularity) gran;
		}

		// create the template and add it for later use
		return new Rasters(bucketSize, level, group);
	}

	@Override
	public boolean isConcreteTemplate() {
		return this.bucketSize != -1;
	}
}
