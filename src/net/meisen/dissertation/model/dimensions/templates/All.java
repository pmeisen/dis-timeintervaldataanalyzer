package net.meisen.dissertation.model.dimensions.templates;

import java.util.Iterator;

import net.meisen.dissertation.model.data.IntervalModel;
import net.meisen.dissertation.model.dimensions.TimeLevelMember;
import net.meisen.dissertation.model.time.mapper.BaseMapper;
import net.meisen.general.genmisc.exceptions.ForwardedRuntimeException;

/**
 * The all level.
 * 
 * @author pmeisen
 * 
 */
public class All extends BaseTimeLevelTemplate {

	@Override
	public String getId() {
		return "*";
	}

	@Override
	public Iterator<TimeLevelMember> it(final IntervalModel model,
			final String timezone) {
		final BaseMapper<?> mapper = model.getTimelineMapper();

		return new Iterator<TimeLevelMember>() {
			private boolean next = true;

			@Override
			public boolean hasNext() {
				return next;
			}

			@Override
			public TimeLevelMember next() {
				next = false;
				return new TimeLevelMember("*", mapper.getNormStartAsLong(),
						mapper.getNormEndAsLong());
			}

			@Override
			public void remove() {
				throw new IllegalStateException("Cannot remove any range.");
			}
		};
	}

	@Override
	public Iterator<TimeLevelMember> it(final IntervalModel model,
			final long start, final long end, final String timezone)
			throws ForwardedRuntimeException {
		return it(model, timezone);
	}
}
