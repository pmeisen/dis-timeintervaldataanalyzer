package net.meisen.dissertation.model.persistence.mock;

import java.io.OutputStream;

import net.meisen.dissertation.model.persistence.BasePersistor;
import net.meisen.dissertation.model.persistence.ILocation;
import net.meisen.dissertation.model.persistence.Identifier;
import net.meisen.dissertation.model.persistence.MetaData;
import net.meisen.general.genmisc.exceptions.registry.IExceptionRegistry;

/**
 * A mock of a {@code BasePersistor}.
 * 
 * @author pmeisen
 * 
 */
public class MockPersistor extends BasePersistor {

	/**
	 * Default constructor.
	 * 
	 * @param exceptionRegistry
	 *            the registry to be used
	 */
	public MockPersistor(final IExceptionRegistry exceptionRegistry) {
		super(exceptionRegistry);
	}

	@Override
	public void save(final ILocation location, final MetaData... additionalData) {
		throw new IllegalStateException(
				"Method 'save' is not implemented and not to be tested");
	}

	@Override
	public void load(final ILocation location, final MetaData... additionalData) {
		throw new IllegalStateException(
				"Method 'save' is not implemented and not to be tested");
	}

	@Override
	protected OutputStream _openForWrite(final Identifier identifier) {
		throw new IllegalStateException(
				"Method 'save' is not implemented and not to be tested");
	}

	@Override
	public void close(final Identifier identifier) {
		throw new IllegalStateException(
				"Method 'save' is not implemented and not to be tested");
	}
}
