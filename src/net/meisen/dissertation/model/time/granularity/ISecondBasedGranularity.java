package net.meisen.dissertation.model.time.granularity;


/**
 * A {@code ITimeGranularity} which can be expressed as a second, i.e. by a
 * fraction or a multiple of a second.
 * 
 * @author pmeisen
 * 
 */
public interface ISecondBasedGranularity extends ITimeGranularity {

	/**
	 * If the granularity can be expressed by an integer of seconds, this method
	 * returns the amount of seconds the granularity stands for, i.e. a minute
	 * has 60 seconds, therefore 60 is returned by such a concrete
	 * implementation. If the granularity represents a fraction of a second,
	 * this method should return {@code -1} and the
	 * {@link #expFractionOfSeconds()} returns the fraction.
	 * 
	 * @return the amount of seconds (integer) represented by one granule of
	 *         this granularity, {@code -1} if it's a fraction and therefore
	 *         cannot be expressed by an integer
	 */
	public int seconds();

	/**
	 * The method returns the (positive) exponent of 10 of the fraction. A
	 * millisecond is a fraction of 1 / 1000 seconds. Therefore the message
	 * returns a 3 ( {@code 10^-3}). The method returns {@code -1} if the
	 * granularity cannot be expressed by a fraction of a second.
	 * 
	 * @return the (positive) exponent of 10 of the fraction, see example
	 */
	public int expFractionOfSeconds();
}
