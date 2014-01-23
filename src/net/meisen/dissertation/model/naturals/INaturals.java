package net.meisen.dissertation.model.naturals;

/**
 * This interface specifies an abstracation considering the used naturals (i.e.
 * {@code Integer}, {@code Long}, {@code BigInteger}).
 * 
 * @author pmeisen
 * 
 * @param <P>
 */
public interface INaturals<P extends INaturals<P>> extends Comparable<P> {

	/**
	 * Subtract the one natural from {@code this}.
	 * 
	 * @param subtrahend
	 *            the natural to be subtracted from {@code this}
	 * 
	 * @return the result of the subtraction
	 */
	public P subtract(final P subtrahend);

	/**
	 * Adds the one natural to {@code this}.
	 * 
	 * @param summand
	 *            the natural to be added
	 * 
	 * @return the result of the addition
	 */
	public P add(final P summand);
}
