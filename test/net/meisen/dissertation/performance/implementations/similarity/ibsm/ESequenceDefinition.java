package net.meisen.dissertation.performance.implementations.similarity.ibsm;

import java.util.Iterator;

import net.meisen.dissertation.impl.parser.query.Interval;
import net.meisen.dissertation.model.data.IntervalModel;
import net.meisen.dissertation.model.time.mapper.BaseMapper;
import net.meisen.dissertation.model.time.timeline.TimelineDefinition;

/**
 * We don't have e-sequences pre-defined. In our definition the e-sequence
 * definition is created dynamically based on a query. A e-sequence definition
 * can be understood as the grouping criteria, specifying which events (records)
 * belong together. <br/>
 * The dynamic definition rasters the time-line into pieces of the size of the
 * specified interval. Thus, it might be necessary to have smaller pieces at the
 * beginning and the end, which is acceptable. And can be skipped later, because
 * it might not be comparable.
 */
public class ESequenceDefinition implements Iterable<long[]> {

	private final long start;
	private final long end;

	private final long windowStart;
	private final long windowEnd;
	private final long windowSize;

	/**
	 * Constructor to create an {@code ESequenceDefinition}.
	 * 
	 * @param intervalModel
	 *            the interval-model used
	 * 
	 * @param interval
	 *            the interval to create the sequence for
	 */
	public ESequenceDefinition(final IntervalModel intervalModel,
			final Interval<?> interval) {
		final BaseMapper<?> mapper = intervalModel.getTimelineMapper();
		final long[] bounds = mapper.getBounds(interval);

		this.windowStart = bounds[0];
		this.windowEnd = bounds[1];
		this.windowSize = this.getWindowEnd() - this.getWindowStart() + 1;

		final TimelineDefinition timeline = intervalModel
				.getTimelineDefinition();
		final long[] timelineBounds = mapper.getBounds(timeline.getStart(),
				timeline.getEnd());

		this.start = timelineBounds[0];
		this.end = timelineBounds[1];
	}

	@Override
	public Iterator<long[]> iterator() {
		final long offset = (this.windowStart - this.start) % this.windowSize;
		final long stepSize = this.windowSize - 1;
		final long firstStart = this.start;
		final long firstEnd = offset == 0 ? stepSize : offset - 1;

		return new Iterator<long[]>() {
			private long nextStart = firstStart;
			private long nextEnd = firstEnd;

			@Override
			public boolean hasNext() {
				return nextStart <= ESequenceDefinition.this.end;
			}

			@Override
			public long[] next() {
				final long[] next = new long[] { nextStart, nextEnd };

				nextStart = nextEnd + 1;
				nextEnd = nextStart + stepSize;
				nextEnd = Math.min(nextEnd, ESequenceDefinition.this.end);

				return next;
			}

			@Override
			public void remove() {
				throw new UnsupportedOperationException(
						"Remove is not supported");

			}
		};
	}

	@SuppressWarnings("javadoc")
	public long getStart() {
		return start;
	}

	@SuppressWarnings("javadoc")
	public long getEnd() {
		return end;
	}

	@SuppressWarnings("javadoc")
	public long getWindowStart() {
		return windowStart;
	}

	@SuppressWarnings("javadoc")
	public long getWindowEnd() {
		return windowEnd;
	}

	@SuppressWarnings("javadoc")
	public long getWindowSize() {
		return windowSize;
	}

	/**
	 * Checks if the size of the window is valid, according to the e-sequence
	 * size.
	 * 
	 * @param window
	 *            the window to be checked
	 * 
	 * @return {@code true} if the size is valid as e-sequence, otherwise
	 *         {@code false}
	 */
	public boolean isValidSize(final long[] window) {
		return getWindowSize() == (window[1] - window[0] + 1);
	}

	@Override
	public String toString() {
		return "[" + getWindowStart() + ", " + getWindowEnd() + "] (size: "
				+ getWindowSize() + ", timeline: [" + getStart() + ", "
				+ getEnd() + "])";
	}
}
