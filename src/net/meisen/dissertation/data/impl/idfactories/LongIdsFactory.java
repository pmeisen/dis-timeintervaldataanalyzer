package net.meisen.dissertation.data.impl.idfactories;

import net.meisen.dissertation.data.IOrderedIdsFactory;
import net.meisen.dissertation.exceptions.IdsFactoryException;
import net.meisen.general.genmisc.exceptions.registry.IExceptionRegistry;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;

/**
 * An implementation of a {@code OrderedIdsFactory}, which is based on
 * {@code Long}.
 * 
 * @see IOrderedIdsFactory
 * 
 * @author pmeisen
 * 
 */
public class LongIdsFactory implements IOrderedIdsFactory<Long> {

	/**
	 * The first generally available identifier
	 */
	protected static final long FIRST_ID = 1l;

	@Autowired
	@Qualifier("exceptionRegistry")
	private IExceptionRegistry exceptionRegistry;

	private long nextId = FIRST_ID;

	@Override
	public void initialize(final Long lastId) {
		if (lastId < FIRST_ID) {
			exceptionRegistry.throwException(IdsFactoryException.class, 1001,
					lastId, FIRST_ID);
		}

		nextId = lastId + 1;
	}

	@Override
	public Class<Long> getIdClass() {
		return Long.class;
	}

	@Override
	public synchronized Long getId() {
		final long id = nextId;

		// check if we had a overflow
		if (id < 0) {
			exceptionRegistry.throwException(IdsFactoryException.class, 1000,
					getClass().getName());
		}

		nextId = nextId + 1;
		return id;
	}

}
