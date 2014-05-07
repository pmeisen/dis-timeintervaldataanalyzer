package net.meisen.dissertation.impl.persistence.mock;

import java.io.IOException;
import java.io.InputStream;

import net.meisen.dissertation.model.persistence.BasePersistor;
import net.meisen.dissertation.model.persistence.Group;
import net.meisen.dissertation.model.persistence.IPersistable;
import net.meisen.dissertation.model.persistence.Identifier;
import net.meisen.general.genmisc.types.Streams;

/**
 * Mock of an {@code Persistable}. The mock loads data from a {@code persistor}
 * 
 * @author pmeisen
 * 
 * @see IPersistable
 * 
 */
public class MockLoadSinglePersistable implements IPersistable {

	private String content;

	@Override
	public void save(final BasePersistor persistor) {
		throw new IllegalStateException("Not Usable for loading.");

	}

	@Override
	public void load(BasePersistor persistor, Identifier identifier,
			InputStream inputStream) {
		try {
			this.content = Streams.readFromStream(inputStream);
		} catch (final IOException e) {
			this.content = null;
		}
	}

	@Override
	public void isRegistered(final BasePersistor persistor, final Group group) {
		// nothing to do
	}

	/**
	 * Gets the content read during loading.
	 * 
	 * @return the content read during loading
	 */
	public String getContent() {
		return this.content;
	}

	@Override
	public Group getPersistentGroup() {
		return null;
	}
}
