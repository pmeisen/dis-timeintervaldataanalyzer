package net.meisen.dissertation.model.persistence.mock;

import net.meisen.dissertation.model.persistence.BasePersistor;
import net.meisen.dissertation.model.persistence.IPersistableFactory;
import net.meisen.dissertation.model.persistence.Identifier;

/**
 * Simple mock for a factory.
 * 
 * @author pmeisen
 * 
 */
public class MockFactory extends MockPersistable implements IPersistableFactory {

	@Override
	public void createInstance(final BasePersistor persistor,
			final Identifier identifier) {
		// nothing
	}
}
