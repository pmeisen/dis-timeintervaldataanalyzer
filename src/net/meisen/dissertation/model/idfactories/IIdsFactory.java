package net.meisen.dissertation.model.idfactories;

/**
 * An {@code IdFactory} is used to create unique identifiers. The factory has to
 * ensure that every call to {@link #getId()} returns a new not prior returned
 * identifier. The general implementation of a {@code IdsFactory} has to ensure
 * that uniqueness across all restarts of the system. Other implementations
 * based on other interfaces (e.g. {@link IOrderedIdsFactory}) may get further
 * information to ensure that uniqueness across restarts.
 * 
 * @author pmeisen
 * 
 * @param <I>
 *            the type of the identifier
 */
public interface IIdsFactory<I extends Object> {

	/**
	 * Get the type (i.e. the {@code Class}) of the identifier.
	 * 
	 * @return the type (i.e. the {@code Class}) of the identifier
	 */
	public Class<I> getIdClass();

	/**
	 * Creates a newly prior not returned identifier. The general implementation
	 * of a {@code IdsFactory} has to ensure that uniqueness across all restarts
	 * of the system.
	 * 
	 * @return a new identifier which wasn't returned before
	 */
	public I getId();

	/**
	 * Sets the specified id as used, so that the factory won't return this
	 * identifier again.
	 * 
	 * @param id
	 *            the identifier to mark as used
	 */
	public void setIdAsUsed(final I id);
}
