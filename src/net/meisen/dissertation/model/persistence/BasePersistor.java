package net.meisen.dissertation.model.persistence;

import java.io.OutputStream;
import java.util.HashMap;
import java.util.Map;

import net.meisen.dissertation.config.xslt.DefaultValues;
import net.meisen.dissertation.exceptions.PersistorException;
import net.meisen.dissertation.model.IPersistable;
import net.meisen.general.genmisc.exceptions.registry.IExceptionRegistry;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;

/**
 * Base implementation for every {@code persistor}.
 * 
 * @author pmeisen
 * 
 */
public abstract class BasePersistor {
	private final static Logger LOG = LoggerFactory
			.getLogger(BasePersistor.class);

	/**
	 * The {@code ExceptionRegistry} used to handle exceptions.
	 */
	@Autowired
	@Qualifier(DefaultValues.EXCEPTIONREGISTRY_ID)
	protected IExceptionRegistry exceptionRegistry;

	private final Map<Group, IPersistable> persistables;

	/**
	 * Default constructor
	 */
	public BasePersistor() {
		persistables = new HashMap<Group, IPersistable>();
	}

	/**
	 * Triggers the saving process by creating a persistence unit (e.g. a file)
	 * under the specified {@code location}.
	 * 
	 * @param location
	 *            the location specifies were to persist the data
	 */
	public abstract void save(final String location);

	/**
	 * Loads the data from the specified {@code location}.
	 * 
	 * @param location
	 *            the location to load the data from
	 */
	public abstract void load(final String location);

	/**
	 * Opens a {@code OutputStream} for the specified {@code identifier}. The
	 * {@code Group} of the {@code identifier} must be a registered one,
	 * otherwise an exception should be thrown.
	 * 
	 * @param identifier
	 *            the {@code Identifier} which identifies the content to be
	 *            written
	 * 
	 * @return the {@code OutputStream} used to write the data
	 */
	public abstract OutputStream openForWrite(final Identifier identifier);

	/**
	 * Closes the stream created for the {@code Identifier}.
	 * 
	 * @param identifier
	 *            the {@code Identifier} which specifies which stream to be
	 *            closed
	 */
	public abstract void close(final Identifier identifier);

	/**
	 * Gets the {@code Persistable} registered for the specified {@code group}.
	 * 
	 * @param group
	 *            the group to get the associated {@code Persistable} for
	 * 
	 * @return the registered {@code Persistable} or {@code null} if none was
	 *         found
	 * 
	 * @see IPersistable
	 */
	protected IPersistable getPersistable(final Group group) {
		return persistables.get(group);
	}

	/**
	 * Get all the registered {@code Persistables} with the {@code Group} each
	 * is bound to.
	 * 
	 * @return all the registered {@code Persistables}
	 * 
	 * @see Group
	 * @see IPersistable
	 */
	protected Map<Group, IPersistable> getPersistables() {
		return persistables;
	}

	/**
	 * Registers a {@code Persistable} to be persisted under the specified
	 * {@code group} name. The method triggers the
	 * {@link IPersistable#isRegistered(BasePersistor, Group)} for each
	 * registration.
	 * 
	 * @param group
	 *            the name of the {@code Group} the {@code Persistable} is bound
	 *            to
	 * @param persistable
	 *            the {@code Persistable} to bound to the specified
	 *            {@code group}
	 */
	public void register(final Group group, final IPersistable persistable) {

		if (persistable == null) {
			exceptionRegistry.throwException(PersistorException.class, 1000);
		} else if (group == null) {
			exceptionRegistry.throwException(PersistorException.class, 1002);
		}

		final IPersistable old = persistables.put(group, persistable);
		if (old != null && old != persistable) {
			exceptionRegistry.throwException(PersistorException.class, 1001,
					group);
		} else if (LOG.isDebugEnabled()) {
			LOG.debug("Bound group '" + group + "' to persistable '"
					+ persistable.getClass().getName() + "'.");
		}

		// fire the event to the registered persistable
		persistable.isRegistered(this, group);
	}

	/**
	 * Unregisters the {@code Persistable} associated to the specified
	 * {@code group}.
	 * 
	 * @param group
	 *            the {@code Persistable} to be removed
	 * 
	 * @return the removed {@code Persistable} might be {@code null} if nothing
	 *         was removed
	 * 
	 * @see IPersistable
	 */
	public IPersistable unregister(final Group group) {
		return persistables.remove(group);
	}
}
