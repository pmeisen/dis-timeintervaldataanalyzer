package net.meisen.dissertation.impl.persistence.mock;

import java.io.InputStream;
import java.io.OutputStream;

import net.meisen.dissertation.model.persistence.BasePersistor;
import net.meisen.dissertation.model.persistence.Group;
import net.meisen.dissertation.model.persistence.IPersistable;
import net.meisen.dissertation.model.persistence.Identifier;
import net.meisen.general.genmisc.types.Streams;

/**
 * Mock of an {@code Persistable}. The mock saves an Identifier in the specified
 * group named {@link MockSaveSinglePersistable#ID}. The content of the entry
 * written is specified by {@link MockSaveSinglePersistable#CONTENT}.
 * 
 * @author pmeisen
 * 
 * @see IPersistable
 * 
 */
public class MockSaveSinglePersistable implements IPersistable {
	/**
	 * The id used
	 */
	public final static String ID = "myId.txt";
	/**
	 * The written content
	 */
	public final static String CONTENT = "Hello from '"
			+ MockSaveSinglePersistable.class.getSimpleName() + "'";

	private Group group;

	@Override
	public void save(final BasePersistor persistor) {
		final Identifier id = new Identifier(ID, group);

		final OutputStream stream = persistor.openForWrite(id);
		Streams.writeStringToStream(CONTENT, stream);
		persistor.close(id);
	}

	@Override
	public void load(final BasePersistor persistor,
			final Identifier identifier, final InputStream inputStream) {
		throw new IllegalStateException("Not Usable for loading.");
	}

	@Override
	public void isRegistered(final BasePersistor persistor, final Group group) {
		this.group = group;
	}

	@Override
	public Group getPersistentGroup() {
		return null;
	}
}
