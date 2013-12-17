package net.meisen.dissertation.models;


/**
 * Interface of a {@code TemporalModel}.
 * 
 * @author pmeisen
 * 
 * @param <T>
 *            the naturals representative used for the temporal model
 */
public interface IRawTemporalModel<T extends INaturals<T>> {

	/**
	 * Gets the type of the {@code TemporalModel}.
	 * 
	 * @return the type of the {@code TemporalModel}
	 */
	public Class<T> getType();

	/**
	 * Gets the granularity for the {@code TemporalModel}.
	 * 
	 * @return the granularity of the {@code TemporalModel}
	 * 
	 * @see IRawTimeGranularity
	 */
	public IRawTimeGranularity getGranularity();

	/**
	 * Returns the start granule for this {@code RawTemporalModel}. The start
	 * value must not be the 'first' value of the underlying {@code Naturals}
	 * nor it must be the 'null' of the underlying {@code Naturals}.
	 * 
	 * @return the first value of the {@code RawTemporalModel}, i.e. other
	 *         values are not covered by this {@code RawTemporalModel}
	 */
	public T getStart();

	/**
	 * Returns the maximal granule represented by this {@code RawTemporalModel}.
	 * The method can return {@code null}. If the model has no other limit than
	 * the space of the memory.
	 * 
	 * @return the maximal value represented by this {@code RawTemporalModel},
	 *         can be {@code null}
	 */
	public T getEnd();

	/**
	 * Get the next granule defined within this {@code RawTemporalModel}.
	 * Because of the nature of computer systems and the fact of the
	 * implementation of a {@code RawTemporalModel} time has to be discrete and
	 * limited. This method returns {@code null} if the limit of the
	 * {@code RawTemporalModel} is reached (i.e. if {@code current.equals(end)}.
	 * 
	 * @param current
	 *            the value to get the next value for
	 * 
	 * @return the next value within this discrete {@code RawTemporalModel}
	 */
	public T getNext(final T current);
}
