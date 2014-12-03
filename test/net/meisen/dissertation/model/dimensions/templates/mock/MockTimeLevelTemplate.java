package net.meisen.dissertation.model.dimensions.templates.mock;

import java.util.Iterator;

import net.meisen.dissertation.model.data.IntervalModel;
import net.meisen.dissertation.model.dimensions.TimeLevelMember;
import net.meisen.dissertation.model.dimensions.templates.ITimeLevelTemplate;
import net.meisen.general.genmisc.exceptions.ForwardedRuntimeException;

/**
 * A mock for testing.
 * 
 * @author pmeisen
 * 
 */
public class MockTimeLevelTemplate implements ITimeLevelTemplate {

	@Override
	public String getId() {
		return "MOCK";
	}

	@Override
	public Iterator<TimeLevelMember> it(final IntervalModel model,
			final String timezone) throws ForwardedRuntimeException {
		return new Iterator<TimeLevelMember>() {

			@Override
			public boolean hasNext() {
				return false;
			}

			@Override
			public TimeLevelMember next() {
				return null;
			}

			@Override
			public void remove() {
				// nothing
			}
		};
	}

	@Override
	public Iterator<TimeLevelMember> it(IntervalModel model, long start,
			long end, String timezone) throws ForwardedRuntimeException {
		return new Iterator<TimeLevelMember>() {

			@Override
			public boolean hasNext() {
				return false;
			}

			@Override
			public TimeLevelMember next() {
				return null;
			}

			@Override
			public void remove() {
				// nothing
			}
		};
	}
}
