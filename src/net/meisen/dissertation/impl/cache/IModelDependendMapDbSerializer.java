package net.meisen.dissertation.impl.cache;

import net.meisen.dissertation.model.data.TidaModel;

import org.mapdb.Serializer;

/**
 * Itnerface to mark a {@code Serializer} of {@code mapDb} to depend on the
 * current {@code TidaModel}.
 * 
 * @author pmeisen
 * 
 * @param <T>
 *            the type of the instance which is serialized
 */
public interface IModelDependendMapDbSerializer<T> extends Serializer<T> {

	/**
	 * Method called whenever the {@code Serializer} is deserialized or created.
	 * 
	 * @param model
	 *            the {@code TidaModel} the {@code Serializer} is used with
	 */
	public void init(final TidaModel model);

}
