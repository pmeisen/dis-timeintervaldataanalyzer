package net.meisen.dissertation.model.dimensions.templates;

import java.util.Iterator;

import net.meisen.dissertation.model.data.IntervalModel;
import net.meisen.dissertation.model.dimensions.TimeLevelMember;
import net.meisen.general.genmisc.exceptions.ForwardedRuntimeException;

/**
 * Interface for a template definition of a {@code TimeLevelTemplate}.
 * 
 * @author pmeisen
 * 
 * @see BaseTimeLevelTemplate
 * 
 */
public interface ITimeLevelTemplate {

	/**
	 * Gets the identifier of the template, useful to re-use the template.
	 * 
	 * @return the identifier of the template
	 */
	public String getId();

	/**
	 * The iterator used to iterate the different {@code TimeMember} instances
	 * of the level. The iterator must iterate over the sorted members (i.e.
	 * sorted by the first range returned by the {@code TimeLevelMember}).
	 * 
	 * @param model
	 *            the {@code IntervalModel} used
	 * @param timezone
	 *            the time-zone used to create the iterator
	 * 
	 * @return the created iterator
	 * 
	 * @throws ForwardedRuntimeException
	 *             if the iterator cannot be created
	 */
	public Iterator<TimeLevelMember> it(final IntervalModel model,
			final String timezone) throws ForwardedRuntimeException;

	/**
	 * Creates an iterator for the specified range.
	 * 
	 * @param model
	 *            the {@code IntervalModel} used
	 * @param start
	 *            the start of the range
	 * @param end
	 *            the end of the range
	 * @param timezone
	 *            the time-zone used to create the iterator
	 * 
	 * @return the created iterator
	 * 
	 * @throws ForwardedRuntimeException
	 *             if the iterator cannot be created
	 */
	public Iterator<TimeLevelMember> it(final IntervalModel model,
			final long start, final long end, final String timezone)
			throws ForwardedRuntimeException;
}