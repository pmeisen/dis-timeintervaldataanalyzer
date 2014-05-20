package net.meisen.dissertation.impl.idfactories;

import net.meisen.dissertation.config.xslt.DefaultValues;
import net.meisen.dissertation.exceptions.IdsFactoryException;
import net.meisen.dissertation.model.idfactories.IOrderedIdsFactory;
import net.meisen.general.genmisc.exceptions.registry.IExceptionRegistry;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;

/**
 * An implementation of a {@code OrderedIdsFactory}, which is based on
 * {@code Integer}.
 * 
 * @see IOrderedIdsFactory
 * 
 * @author pmeisen
 * 
 */
public class ByteIdsFactory implements IOrderedIdsFactory<Byte> {

	/**
	 * The first generally available identifier
	 */
	protected static final byte FIRST_ID = 1;

	@Autowired
	@Qualifier(DefaultValues.EXCEPTIONREGISTRY_ID)
	private IExceptionRegistry exceptionRegistry;

	private byte nextId = FIRST_ID;

	@Override
	public void initialize(final Byte lastId) {
		if (lastId < FIRST_ID) {
			exceptionRegistry.throwException(IdsFactoryException.class, 1001,
					lastId, FIRST_ID);
		}

		nextId = (byte) (lastId + 1);
	}

	@Override
	public Class<Byte> getIdClass() {
		return Byte.class;
	}

	@Override
	public synchronized Byte getId() {
		final byte id = nextId;

		// check if we had an overflow
		if (id < 0) {
			exceptionRegistry.throwException(IdsFactoryException.class, 1000,
					getClass().getName());
		}

		nextId++;
		return id;
	}

	@Override
	public void setIdAsUsed(final Byte id) {
		if (id < nextId) {
			return;
		} else {
			nextId = (byte) (id + 1);
		}
	}
}
