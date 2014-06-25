package net.meisen.dissertation.impl.cache;

import java.io.Serializable;

import net.meisen.dissertation.model.data.TidaModel;
import net.meisen.general.genmisc.exceptions.ForwardedRuntimeException;

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
public interface IModelDependendMapDbSerializer<T> extends Serializer<T>,
		Serializable {

	/**
	 * Method called whenever the {@code Serializer} is deserialized or created.
	 * 
	 * @param cache
	 *            the cache, which calls the initialization
	 * @param model
	 *            the {@code TidaModel} the {@code Serializer} is used with
	 * 
	 * @throws ForwardedRuntimeException
	 *             if an error during initialization occures
	 */
	public void init(final BaseMapDbCache<?, ?> cache, final TidaModel model)
			throws ForwardedRuntimeException;

}
