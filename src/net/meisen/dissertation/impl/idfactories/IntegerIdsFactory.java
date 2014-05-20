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
public class IntegerIdsFactory implements IOrderedIdsFactory<Integer> {

	/**
	 * The first generally available identifier
	 */
	protected static final int FIRST_ID = 1;

	@Autowired
	@Qualifier(DefaultValues.EXCEPTIONREGISTRY_ID)
	private IExceptionRegistry exceptionRegistry;

	private int nextId = FIRST_ID;

	@Override
	public void initialize(final Integer lastId) {
		if (lastId < FIRST_ID) {
			exceptionRegistry.throwException(IdsFactoryException.class, 1001,
					lastId, FIRST_ID);
		}

		nextId = lastId + 1;
	}

	@Override
	public Class<Integer> getIdClass() {
		return Integer.class;
	}

	@Override
	public synchronized Integer getId() {
		final int id = nextId;

		// check if we had an overflow
		if (id < 0) {
			exceptionRegistry.throwException(IdsFactoryException.class, 1000,
					getClass().getName());
		}

		nextId = nextId + 1;
		return id;
	}

	@Override
	public void setIdAsUsed(final Integer id) {
		if (id < nextId) {
			return;
		} else {
			nextId = id + 1;
		}
	}
}
