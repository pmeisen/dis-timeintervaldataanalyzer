package net.meisen.dissertation.model.idfactories;


/**
 * An {@code IdsFactory} which uses an ordered list for identifiers to be
 * created.
 * 
 * @see IIdsFactory
 * 
 * @author pmeisen
 * 
 * @param <I>
 */
public interface IOrderedIdsFactory<I extends Object> extends IIdsFactory<I> {

	/**
	 * Define the last identifier assign by this {@code OrderedIdsFactory}. This
	 * method should be used to initialize the factory when reloading the data.
	 * 
	 * @param lastId
	 *            the last assigned and therefore unavailable identifier
	 */
	public void initialize(final I lastId);
}
