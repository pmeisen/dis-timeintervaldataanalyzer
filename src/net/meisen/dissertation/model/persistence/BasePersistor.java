package net.meisen.dissertation.model.persistence;

import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
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
	 * Write the specified {@code object} to the {@code stream}.
	 * 
	 * @param stream
	 *            the stream to write to
	 * @param object
	 *            the object to be written
	 * 
	 * @throws IOException
	 *             if the writing fails
	 */
	public void writeObject(final OutputStream stream, final Object object)
			throws IOException {
		final ObjectOutputStream oos = new ObjectOutputStream(stream);
		oos.writeObject(object);
	}

	/**
	 * Write the specified {@code int} to the {@code stream}.
	 * 
	 * @param stream
	 *            the stream to write to
	 * @param value
	 *            the value to be written
	 * 
	 * @throws IOException
	 *             if the writing fails
	 */
	public void writeInt(final OutputStream stream, final int value)
			throws IOException {
		final DataOutputStream dos = new DataOutputStream(stream);
		dos.writeInt(value);
	}

	/**
	 * Write the specified {@code long} to the {@code stream}.
	 * 
	 * @param stream
	 *            the stream to write to
	 * @param value
	 *            the value to be written
	 * 
	 * @throws IOException
	 *             if the writing fails
	 */
	public void writeLong(final OutputStream stream, final long value)
			throws IOException {
		final DataOutputStream dos = new DataOutputStream(stream);
		dos.writeLong(value);
	}

	/**
	 * Write the specified {@code short} to the {@code stream}.
	 * 
	 * @param stream
	 *            the stream to write to
	 * @param value
	 *            the value to be written
	 * 
	 * @throws IOException
	 *             if the writing fails
	 */
	public void writeShort(final OutputStream stream, final short value)
			throws IOException {
		final DataOutputStream dos = new DataOutputStream(stream);
		dos.writeShort(value);
	}

	/**
	 * Write the specified {@code byte} to the {@code stream}.
	 * 
	 * @param stream
	 *            the stream to write to
	 * @param value
	 *            the value to be written
	 * 
	 * @throws IOException
	 *             if the writing fails
	 */
	public void writeByte(final OutputStream stream, final byte value)
			throws IOException {
		final DataOutputStream dos = new DataOutputStream(stream);
		dos.writeByte(value);
	}

	/**
	 * Write the specified {@code String} to the {@code stream}.
	 * 
	 * @param stream
	 *            the stream to write to
	 * @param value
	 *            the value to be written
	 * 
	 * @throws IOException
	 *             if the writing fails
	 */
	public void writeString(final OutputStream stream, final String value)
			throws IOException {
		final DataOutputStream dos = new DataOutputStream(stream);
		dos.writeUTF(value);
	}

	/**
	 * Reads an object from the {@code stream}.
	 * 
	 * @param stream
	 *            the stream to read from
	 * 
	 * @return the object read
	 * 
	 * @throws IOException
	 *             if the reading fails
	 * @throws ClassNotFoundException
	 *             if the class of the object cannot be found
	 */
	public Object readObject(final InputStream stream) throws IOException,
			ClassNotFoundException {
		final ObjectInputStream ois = new ObjectInputStream(stream);
		return ois.readObject();
	}

	/**
	 * Reads an {@code int} from the {@code stream}.
	 * 
	 * @param stream
	 *            the stream to read from
	 * 
	 * @return the {@code int} read
	 * 
	 * @throws IOException
	 *             if the reading fails
	 */
	public int readInt(final InputStream stream) throws IOException {
		final DataInputStream dis = new DataInputStream(stream);
		return dis.readInt();
	}

	/**
	 * Reads an {@code long} from the {@code stream}.
	 * 
	 * @param stream
	 *            the stream to read from
	 * 
	 * @return the {@code long} read
	 * 
	 * @throws IOException
	 *             if the reading fails
	 */
	public long readLong(final InputStream stream) throws IOException {
		final DataInputStream dis = new DataInputStream(stream);
		return dis.readLong();
	}

	/**
	 * Reads an {@code short} from the {@code stream}.
	 * 
	 * @param stream
	 *            the stream to read from
	 * 
	 * @return the {@code short} read
	 * 
	 * @throws IOException
	 *             if the reading fails
	 */
	public short readShort(final InputStream stream) throws IOException {
		final DataInputStream dis = new DataInputStream(stream);
		return dis.readShort();
	}

	/**
	 * Reads an {@code byte} from the {@code stream}.
	 * 
	 * @param stream
	 *            the stream to read from
	 * 
	 * @return the {@code byte} read
	 * 
	 * @throws IOException
	 *             if the reading fails
	 */
	public byte readByte(final InputStream stream) throws IOException {
		final DataInputStream dis = new DataInputStream(stream);
		return dis.readByte();
	}

	/**
	 * Reads an {@code String} from the {@code stream}.
	 * 
	 * @param stream
	 *            the stream to read from
	 * 
	 * @return the {@code String} read
	 * 
	 * @throws IOException
	 *             if the reading fails
	 */
	public String readString(final InputStream stream) throws IOException {
		final DataInputStream dis = new DataInputStream(stream);
		return dis.readUTF();
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
