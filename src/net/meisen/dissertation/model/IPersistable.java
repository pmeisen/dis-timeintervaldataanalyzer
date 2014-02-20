package net.meisen.dissertation.model;

import java.io.InputStream;

import net.meisen.dissertation.model.persistence.BasePersistor;
import net.meisen.dissertation.model.persistence.Group;
import net.meisen.dissertation.model.persistence.Identifier;
import net.meisen.general.genmisc.exceptions.ForwardedRuntimeException;

public interface IPersistable {

	public void save(final BasePersistor persistor)
			throws ForwardedRuntimeException;

	public void load(final BasePersistor persistor,
			final Identifier identifier, final InputStream inputStream)
			throws ForwardedRuntimeException;

	public void isRegistered(final BasePersistor persistor, final Group group);
	
	/**
	 * Gets the group used to persist the data.
	 * 
	 * @return the group used to persist the data
	 */
	public Group getPersistentGroup();
}
