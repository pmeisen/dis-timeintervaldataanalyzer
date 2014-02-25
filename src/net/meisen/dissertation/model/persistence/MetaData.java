package net.meisen.dissertation.model.persistence;

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.io.InputStream;

import net.meisen.general.genmisc.types.Streams;

/**
 * This class is used to represent some meta information which should be
 * persisted but doesn't belong to any instance (e.g. a version) or should be
 * persisted prior to any existence of the system (e.g. the system's
 * configuration).
 * 
 * @author pmeisen
 * 
 */
public class MetaData {

	private final Identifier identifier;
	private byte[] data = null;

	/**
	 * Creates a {@code MetaData} used to persist additional data.
	 * 
	 * @param identifier
	 *            the identifier used to identify the data
	 * @param data
	 *            the data to be persisted
	 */
	public MetaData(final Identifier identifier, final String data) {
		this.identifier = identifier;
		this.data = data.getBytes();
	}

	/**
	 * Creates a {@code MetaData} used to persist additional data.
	 * 
	 * @param identifier
	 *            the identifier used to identify the data
	 * @param data
	 *            the data to be persisted
	 */
	public MetaData(final Identifier identifier, final InputStream data) {
		this.identifier = identifier;
		setData(data);
	}

	/**
	 * Creates a {@code MetaData} used to persist additional data.
	 * 
	 * @param identifier
	 *            the identifier used to identify the data
	 * @param data
	 *            the data to be persisted
	 */
	public MetaData(final Identifier identifier, final byte[] data) {
		this.identifier = identifier;
		setData(data);
	}

	/**
	 * Creates a {@code MetaData} used to load additional data.
	 * 
	 * @param identifier
	 *            the identifier which identifies which data to be loaded
	 */
	public MetaData(final Identifier identifier) {
		this.identifier = identifier;
	}

	/**
	 * Method used to set the data loaded or defined for the {@code MetaData}.
	 * 
	 * @param data
	 *            the data to be set
	 */
	public void setData(final InputStream data) {
		try {
			setData(Streams.copyStreamToByteArray(data));
		} catch (final IOException e) {
			throw new IllegalArgumentException(
					"The specified data cannot be read.");
		}
	}

	/**
	 * Method used to set the data loaded or defined for the {@code MetaData}.
	 * 
	 * @param data
	 *            the data to be set
	 */
	public void setData(final byte[] data) {
		this.data = data;
	}

	/**
	 * Gets the loaded or defined data.
	 * 
	 * @return the loaded or defined data
	 */
	public byte[] getData() {
		return data;
	}

	/**
	 * Get the identifier used to identify the {@code MetaData}.
	 * 
	 * @return the identifier used to identify the {@code MetaData}
	 */
	public Identifier getIdentifier() {
		return identifier;
	}

	/**
	 * Gets a stream which can be used to read the data of {@code this}.
	 * 
	 * @return a stream to read the data
	 */
	public InputStream getStream() {
		return new ByteArrayInputStream(data == null ? new byte[0] : data);
	}
}
