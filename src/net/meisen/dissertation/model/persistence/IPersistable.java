package net.meisen.dissertation.model.persistence;

import java.io.InputStream;

import net.meisen.general.genmisc.exceptions.ForwardedRuntimeException;

/**
 * Interface used to mark a object as persistable, i.e. an object which can be
 * registered at a {@code Persistor}.
 * 
 * @author pmeisen
 * 
 * @see BasePersistor
 * 
 */
public interface IPersistable {

	/**
	 * This method is called by the {@code persistor} if the instance should be
	 * saved. The {@code persistor} offers methods like
	 * {@link BasePersistor#openForWrite(Identifier)} to open a stream to write
	 * to the persistance-layer. After successful loading the
	 * {@link BasePersistor#close(Identifier)} has to be called.
	 * 
	 * @param persistor
	 *            the {@code Persistor} which triggered the saving
	 * 
	 * @throws ForwardedRuntimeException
	 *             if an exception occurs
	 */
	public void save(final BasePersistor persistor)
			throws ForwardedRuntimeException;

	/**
	 * This method is called by the {@code persistor} to load the data from the
	 * {@code inputStream} for the specified {@code identifier}.
	 * 
	 * @param persistor
	 *            the {@code Persistor} which triggered the loading
	 * @param identifier
	 *            the {@code Identifier} which was defined when saving
	 * @param inputStream
	 *            the {@code InputStream} to read from
	 * 
	 * @throws ForwardedRuntimeException
	 *             if an exception occurs
	 */
	public void load(final BasePersistor persistor,
			final Identifier identifier, final InputStream inputStream)
			throws ForwardedRuntimeException;

	/**
	 * This method is called by the {@code persistor} if the instance was
	 * registered under the specified {@code group}.
	 * 
	 * @param persistor
	 *            the {@code Persistor} at which the {@code Persistable} was
	 *            registered at
	 * @param group
	 *            the group the {@code Persistor} was registered under
	 */
	public void isRegistered(final BasePersistor persistor, final Group group);

	/**
	 * Gets the group used to persist the data.
	 * 
	 * @return the group used to persist the data
	 */
	public Group getPersistentGroup();
}
