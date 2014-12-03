package net.meisen.dissertation.model.dimensions.templates;

import java.util.Iterator;

import net.meisen.dissertation.exceptions.TimeDimensionException;
import net.meisen.dissertation.model.data.IntervalModel;
import net.meisen.dissertation.model.dimensions.TimeLevelMember;
import net.meisen.dissertation.model.time.granularity.IDateBasedGranularity;
import net.meisen.dissertation.model.time.granularity.ITimeGranularity;
import net.meisen.dissertation.model.time.granularity.Second;
import net.meisen.general.genmisc.exceptions.ForwardedRuntimeException;

/**
 * A template for a seconds-level.
 * 
 * @author pmeisen
 * 
 */
public class Seconds extends BaseTimeLevelTemplate {

	@Override
	public String getId() {
		return "SECONDS";
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

		final ITimeGranularity granularity = model.getTimelineDefinition()
				.getGranularity();

		// make sure we are date-based
		if (granularity instanceof IDateBasedGranularity == false) {
			throw new ForwardedRuntimeException(TimeDimensionException.class,
					1000, granularity.getClass().getSimpleName(), getClass()
							.getSimpleName());
		} else if (!((IDateBasedGranularity) granularity).isAssignableTo('s')) {
			throw new ForwardedRuntimeException(TimeDimensionException.class,
					1000, granularity.getClass().getSimpleName(), getClass()
							.getSimpleName());
		}
	}

	@Override
	public Iterator<TimeLevelMember> it(final IntervalModel model,
			final String timezone) throws ForwardedRuntimeException {
		validate(model);
		return createIterator(model, timezone, Second.instance());
	}

	@Override
	public Iterator<TimeLevelMember> it(final IntervalModel model,
			final long start, final long end, final String timezone)
			throws ForwardedRuntimeException {
		validate(model);
		return createIterator(model, timezone, Second.instance(), start, end);
	}
}
