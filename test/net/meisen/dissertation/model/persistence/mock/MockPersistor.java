package net.meisen.dissertation.model.persistence.mock;

import java.io.OutputStream;

import net.meisen.dissertation.model.persistence.BasePersistor;
import net.meisen.dissertation.model.persistence.Identifier;

/**
 * A mock of a {@code BasePersistor}.
 * 
 * @author pmeisen
 * 
 */
public class MockPersistor extends BasePersistor {

	@Override
	public void save(final String location) {
		throw new IllegalStateException(
				"Method 'save' is not implemented and not to be tested");
	}

	@Override
	public void load(final String location) {
		throw new IllegalStateException(
				"Method 'save' is not implemented and not to be tested");
	}

	@Override
	public OutputStream openForWrite(final Identifier identifier) {
		throw new IllegalStateException(
				"Method 'save' is not implemented and not to be tested");
	}

	@Override
	public void close(final Identifier identifier) {
		throw new IllegalStateException(
				"Method 'save' is not implemented and not to be tested");
	}
}
