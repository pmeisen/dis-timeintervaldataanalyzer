package net.meisen.dissertation.impl.parser.query.select;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import net.meisen.dissertation.impl.parser.query.Interval;
import net.meisen.dissertation.model.data.IntervalModel;
import net.meisen.dissertation.model.indexes.BaseIndexFactory;
import net.meisen.dissertation.model.indexes.datarecord.TidaIndex;
import net.meisen.dissertation.model.indexes.datarecord.slices.Bitmap;
import net.meisen.dissertation.model.indexes.datarecord.slices.SliceWithDescriptors;
import net.meisen.dissertation.model.time.mapper.BaseMapper;

/**
 * Enum definition of the different results possible when querying for data.
 * 
 * @author pmeisen
 * 
 */
public enum IntervalRelation {
	/**
	 * The equal-relation searches for intervals which are equal to the
	 * specified time-window, i.e. start and end are equal.
	 */
	EQUALTO,
	/**
	 * Before searches for all intervals within the database, which are before
	 * the specified time-window. If the time-window exceeds the defined
	 * time-line of the database all records are returned. If the time-window is
	 * below the defined time-line nothing is returned.<br/>
	 * <br/>
	 * A time-window of {@code [5, 8]} selects {@code [0, 4]}, {@code [0, 3]},
	 * {@code [3, 3]}, it does not select {@code [2, 5]}.
	 */
	BEFORE,
	/**
	 * After searches for all intervals within the database, which are after the
	 * specified time-window. If the time-window exceeds the defined time-line
	 * of the database no records are returned. On the other hand, if the
	 * time-window is below the defined time-line all records are returned.<br/>
	 * <br/>
	 * 
	 * <b>Example:</b><br/>
	 * A time-window of {@code [5, 8]} selects {@code [9, 9]}, {@code [12, 13]},
	 * {@code [13, 32]}, it does not select {@code [8, 10]}.
	 */
	AFTER,
	/**
	 * The meeting-relationships searches for intervals, which are directly
	 * connecting, but do not overlap with the time-window.<br/>
	 * <br/>
	 * 
	 * <b>Example:</b><br/>
	 * A time-window of {@code [5, 8]} selects {@code [3, 4]}, {@code [4, 4]},
	 * {@code [9, 12]}, it does not select {@code [8, 10]}, {@code [10, 10]}.
	 */
	MEETING,
	/**
	 * The overlapping relation returns all the records which are overlap the
	 * left- or right-side of the time-window. Overlapping means that the
	 * interval overlaps only on one of the sides of the time-window, and not
	 * both (that relation is called {@link #CONTAINING}). If the time-window is
	 * below or exceeds the defined time-line of the database no records are
	 * returned for that side.<br/>
	 * <br/>
	 * 
	 * <b>Example:</b><br/>
	 * A time-window of {@code [5, 8]} on a time-line {@code [0, 7]} selects
	 * {@code [4, 6]}, {@code [4, 7]}, {@code [0, 7]}, it does not select
	 * {@code [4, 5]}, {@code [5, 7]} , {@code [7, 7]}.
	 */
	OVERLAPPING, DURING, CONTAINING, STARTINGWITH, FINISHINGWITH, WITHIN;

	private final static Logger LOG = LoggerFactory
			.getLogger(IntervalRelation.class);

	public Bitmap determine(final IntervalModel intervalModel,
			final TidaIndex index, final Interval timeWindow) {
		final BaseMapper<?> mapper = intervalModel.getTimelineMapper();
		final BaseIndexFactory indexFactory = intervalModel.getIndexFactory();

		// the result
		final Bitmap bitmap;

		// make sure the interval is valid
		if (timeWindow != null && mapper.isLargerThanEnd(timeWindow.getStart())) {
			if (BEFORE.equals(this)) {
				bitmap = combineOr(index.getIntervalIndexSlices());
			} else {
				bitmap = null;
			}
		} else if (timeWindow != null
				&& mapper.isSmallerThanStart(timeWindow.getEnd())) {
			if (AFTER.equals(this)) {
				bitmap = combineOr(index.getIntervalIndexSlices());
			} else {
				bitmap = null;
			}
		} else {

			/*
			 * get the mapped start (included) and end (included) of the
			 * interval
			 */
			final long lStart;
			final long lEnd;
			if (timeWindow == null) {
				lStart = mapper.getNormStartAsLong();
				lEnd = mapper.getNormEndAsLong();
			} else {
				lStart = timeWindow.getOpenType().isInclusive() ? mapper
						.mapToLong(timeWindow.getStart()) : mapper.shiftToLong(
						timeWindow.getStart(), 1, false);
				lEnd = timeWindow.getCloseType().isInclusive() ? mapper
						.mapToLong(timeWindow.getEnd()) : mapper.shiftToLong(
						timeWindow.getEnd(), 1, true);
			}

			// make sure the values are valid
			if (lEnd < lStart) {
				bitmap = null;
			} else {

				switch (this) {
				case WITHIN:
					bitmap = withIn(index, lStart, lEnd);
					break;
				case OVERLAPPING:
					bitmap = overlapping(index, lStart, lEnd, mapper);
					break;
				case DURING:
					bitmap = during(index, lStart, lEnd, mapper, timeWindow);
					break;
				case STARTINGWITH:
					bitmap = startingWith(index, lStart, mapper, timeWindow);
					break;
				case FINISHINGWITH:
					bitmap = finishingWith(index, lEnd, mapper, timeWindow);
					break;
				case EQUALTO:
					bitmap = equalTo(index, lStart, lEnd, mapper, timeWindow);
					break;
				case BEFORE:
					bitmap = before(index, lStart, mapper);
					break;
				case AFTER:
					bitmap = after(index, lEnd, mapper);
					break;
				case MEETING:
					bitmap = meeting(index, lStart, lEnd, mapper, timeWindow);
					break;
				case CONTAINING:
					bitmap = containing(index, lStart, lEnd, mapper);
					break;
				default:
					if (LOG.isWarnEnabled()) {
						LOG.warn("Implementation of IntervalRelation '" + this
								+ "' is missing.");
					}
					bitmap = null;
					break;
				}
			}
		}

		// if there is no bitmap so far, return an empty one
		if (bitmap == null) {
			return indexFactory.createBitmap();
		} else {
			return bitmap;
		}
	}

	protected Bitmap containing(final TidaIndex index, final long lStart,
			final long lEnd, final BaseMapper<?> mapper) {
		final long tStart = mapper.getNormStartAsLong();
		final long tEnd = mapper.getNormEndAsLong();

		if (lStart == tStart || lEnd == tEnd) {
			return null;
		} else {
			final Bitmap prevStart = getBitmap(index, lStart - 1);
			final Bitmap start = getBitmap(index, lStart);
			final Bitmap end = getBitmap(index, lEnd);
			final Bitmap follEnd = getBitmap(index, lEnd + 1);

			return Bitmap.and(index.getIndexFactory(), prevStart, start, end,
					follEnd);
		}
	}

	protected Bitmap meeting(final TidaIndex index, final long lStart,
			final long lEnd, final BaseMapper<?> mapper,
			final Interval timeWindow) {

		final Bitmap attachedAtStart;
		if (mapper.getNormStartAsLong() == lStart) {
			attachedAtStart = null;
		} else {
			attachedAtStart = finishingWith(index, lStart - 1, mapper,
					timeWindow);
		}

		final Bitmap attachedAtEnd;
		if (mapper.getNormEndAsLong() == lEnd) {
			attachedAtEnd = null;
		} else {
			attachedAtEnd = startingWith(index, lEnd + 1, mapper, timeWindow);
		}

		// determine the result
		if (attachedAtStart != null && attachedAtEnd != null) {
			return attachedAtStart.or(attachedAtEnd);
		} else if (attachedAtStart != null) {
			return attachedAtEnd;
		} else {
			return attachedAtStart;
		}
	}

	protected Bitmap overlapping(final TidaIndex index, final long lStart,
			final long lEnd, final BaseMapper<?> mapper) {

		// get the edges of time
		final long tStart = mapper.getNormStartAsLong();
		final long tEnd = mapper.getNormEndAsLong();

		final Bitmap startOverlapping;
		if (tStart == lStart || tEnd == lStart) {
			startOverlapping = null;
		} else {
			startOverlapping = combineAnd(index.getIntervalIndexSlices(
					lStart - 1, lStart + 1));
		}

		final Bitmap endOverlapping;
		if (tEnd == lEnd || tStart == lEnd) {
			endOverlapping = null;
		} else {
			endOverlapping = combineAnd(index.getIntervalIndexSlices(lEnd - 1,
					lEnd + 1));
		}

		// determine the result
		if (startOverlapping != null && endOverlapping != null) {
			return startOverlapping.xor(endOverlapping);
		} else if (startOverlapping != null) {
			return startOverlapping;
		} else {
			return endOverlapping;
		}
	}

	protected Bitmap during(final TidaIndex index, final long lStart,
			final long lEnd, final BaseMapper<?> mapper,
			final Interval timeWindow) {
		final long tStart = mapper.getNormStartAsLong();
		final long tEnd = mapper.getNormEndAsLong();

		final long start;
		if ((timeWindow == null && lStart == tStart)
				|| (timeWindow != null && mapper.isSmallerThanStart(timeWindow
						.getStart()))) {
			start = tStart;
		} else {
			start = lStart + 1;
		}

		final long end;
		if ((timeWindow == null && lEnd == tEnd)
				|| (timeWindow != null && mapper.isLargerThanEnd(timeWindow
						.getEnd()))) {
			end = tEnd;
		} else {
			end = lEnd - 1;
		}

		// get all the once within
		Bitmap bitmap = withIn(index, start, end);
		if (bitmap == null) {
			return null;
		}

		// remove the once overlapping the start
		if (start != tStart) {

			// get the Bitmaps overlapping the start
			final Bitmap prevBitmap = getBitmap(index, lStart);
			bitmap = bitmap.xor(prevBitmap).and(bitmap);
		}

		// remove the once overlapping the end
		if (end != tEnd) {

			// get the Bitmaps overlapping the end
			final Bitmap follBitmap = getBitmap(index, lEnd);
			bitmap = bitmap.xor(follBitmap).and(bitmap);
		}

		return bitmap;
	}

	protected Bitmap before(final TidaIndex index, final long lStart,
			final BaseMapper<?> mapper) {
		final long tStart = mapper.getNormStartAsLong();

		if (tStart < lStart) {
			return during(index, tStart, lStart, mapper, null);
		} else {
			return null;
		}
	}

	protected Bitmap after(final TidaIndex index, final long lEnd,
			final BaseMapper<?> mapper) {
		final long tEnd = mapper.getNormEndAsLong();

		if (tEnd > lEnd) {
			return during(index, lEnd, tEnd, mapper, null);
		} else {
			return null;
		}
	}

	protected Bitmap startingWith(final TidaIndex index, final long lStart,
			final BaseMapper<?> mapper, final Interval<?> timeWindow) {

		if (timeWindow != null
				&& mapper.isSmallerThanStart(timeWindow.getStart())) {
			return null;
		} else if (mapper.getNormStartAsLong() == lStart) {
			final SliceWithDescriptors<?>[] finishSlices = index
					.getIntervalIndexSlices(lStart, lStart);
			return getBitmap(finishSlices, 0, 1);
		} else {

			// get the Bitmaps of the slices
			final Bitmap prevBitmap = getBitmap(index, lStart - 1);
			final Bitmap startBitmap = getBitmap(index, lStart);

			if (startBitmap == null) {
				return null;
			} else {
				return startBitmap.xor(prevBitmap).and(startBitmap);
			}
		}
	}

	protected Bitmap finishingWith(final TidaIndex index, final long lEnd,
			final BaseMapper<?> mapper, final Interval<?> timeWindow) {

		if (timeWindow != null && mapper.isLargerThanEnd(timeWindow.getEnd())) {
			return null;
		} else if (mapper.getNormEndAsLong() == lEnd) {
			final SliceWithDescriptors<?>[] finishSlices = index
					.getIntervalIndexSlices(lEnd, lEnd);
			return getBitmap(finishSlices, 0, 1);
		} else {

			// get the Bitmaps of the slices
			final Bitmap finishBitmap = getBitmap(index, lEnd);
			final Bitmap follBitmap = getBitmap(index, lEnd + 1);

			if (finishBitmap == null) {
				return null;
			} else {
				return finishBitmap.xor(follBitmap).and(finishBitmap);
			}
		}
	}

	protected Bitmap equalTo(final TidaIndex index, final long lStart,
			final long lEnd, final BaseMapper<?> mapper,
			final Interval<?> timeWindow) {
		final Bitmap startWith = startingWith(index, lStart, mapper, timeWindow);
		final Bitmap finishWith = finishingWith(index, lEnd, mapper, timeWindow);

		if (startWith == null || finishWith == null) {
			return null;
		} else {
			return startWith.and(finishWith);
		}
	}

	protected Bitmap withIn(final TidaIndex index, final long lStart,
			final long lEnd) {
		return combineOr(index.getIntervalIndexSlices(lStart, lEnd));
	}

	protected Bitmap combineOr(final SliceWithDescriptors<?>[] slices) {
		Bitmap bitmap = null;

		if (slices != null && slices.length >= 0) {
			for (final SliceWithDescriptors<?> timeSlice : slices) {
				final Bitmap timeSliceBitmap = timeSlice.getBitmap();

				// check if there is a bitmap defined
				if (timeSliceBitmap != null) {
					if (bitmap == null) {
						bitmap = timeSliceBitmap;
					} else {
						bitmap = bitmap.or(timeSliceBitmap);
					}
				}
			}
		}

		return bitmap;
	}

	protected Bitmap combineAnd(final SliceWithDescriptors<?>[] slices) {
		Bitmap bitmap = null;

		if (slices != null && slices.length >= 0) {
			for (final SliceWithDescriptors<?> timeSlice : slices) {
				final Bitmap timeSliceBitmap = timeSlice.getBitmap();

				// check if there is a bitmap defined
				if (timeSliceBitmap == null) {
					return null;
				} else {
					if (bitmap == null) {
						bitmap = timeSliceBitmap;
					} else {
						bitmap = bitmap.and(timeSliceBitmap);
					}
				}
			}
		}

		return bitmap;
	}

	protected Bitmap getBitmap(final TidaIndex index, final long pos) {
		final SliceWithDescriptors<?>[] slices = index.getIntervalIndexSlices(
				pos, pos);
		return getBitmap(slices, 0, 1);
	}

	protected Bitmap getBitmap(final SliceWithDescriptors<?>[] slices,
			final int pos, final int expectedLength) {

		if (slices == null) {
			return null;
		} else if (expectedLength > -1 && slices.length != expectedLength) {
			return null;
		} else {
			return getSliceBitmap(slices[pos]);
		}
	}

	protected Bitmap getSliceBitmap(final SliceWithDescriptors<?> slice) {
		if (slice == null) {
			return null;
		} else {
			return slice.getBitmap();
		}
	}
}
