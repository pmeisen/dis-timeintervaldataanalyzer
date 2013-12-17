package net.meisen.dissertation.models;

/**
 * A factory to create {@code Naturals} of a specific type.
 * 
 * @author pmeisen
 * 
 * @param <T>
 *            the {@code Naturals} to be created by the implementation
 */
public interface INaturalsFactory<T extends INaturals<T>> {

	/**
	 * Gets the natural representing zero.
	 * 
	 * @return the natural representing zero
	 */
	public T getZero();

	/**
	 * Gets the natural representing one.
	 * 
	 * @return the natural representing one
	 */
	public T getOne();

	/**
	 * Gets the maximal natural representable. Can be {@code null} if there is
	 * no limit (besides the physical limits of a computer} defined.
	 * 
	 * @return the maximal natural
	 */
	public T getMax();

	/**
	 * Gets a random natural.
	 * 
	 * @return a random natural
	 */
	public T getRandom();

	/**
	 * Generates a natural based on the passed {@code representative}. Please
	 * have a look at the documentation of the concrete implementation to
	 * identify which types are supported.
	 * 
	 * @param representative
	 *            the representative to be transformed into a natural
	 * 
	 * @return the natural which is represented by the passed
	 *         {@code representative}
	 */
	public T generate(final Object representative);

	/**
	 * Get the type of the {@code Naturals} implemented
	 * 
	 * @return the type of the {@code Naturals}
	 */
	public Class<T> getNaturalsType();

	/**
	 * Gets the internal Java type used to represent the naturals.
	 * 
	 * @return the internal Java type used to represent the naturals
	 */
	public Class<?> getJavaType();
}
