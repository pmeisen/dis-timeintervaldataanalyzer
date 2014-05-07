package net.meisen.dissertation.model.persistence.mock;

import java.io.InputStream;

import net.meisen.dissertation.model.persistence.BasePersistor;
import net.meisen.dissertation.model.persistence.Group;
import net.meisen.dissertation.model.persistence.IPersistable;
import net.meisen.dissertation.model.persistence.Identifier;

/**
 * A mock of a {@code Persistable}.
 * 
 * @author pmeisen
 * 
 * @see IPersistable
 * 
 */
public class MockPersistable implements IPersistable {

	@Override
	public void save(final BasePersistor persistor) {
		// nothing
	}

	@Override
	public void load(final BasePersistor persistor, final Identifier id,
			final InputStream inputStream) {
		// nothing
	}

	@Override
	public void isRegistered(final BasePersistor persistor, Group group) {
		// nothing
	}

	@Override
	public Group getPersistentGroup() {
		return null;
	}
}
