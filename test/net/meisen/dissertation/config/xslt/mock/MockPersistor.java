package net.meisen.dissertation.config.xslt.mock;

import java.io.OutputStream;

import net.meisen.dissertation.model.persistence.BasePersistor;
import net.meisen.dissertation.model.persistence.Identifier;
import net.meisen.dissertation.model.persistence.ILocation;
import net.meisen.dissertation.model.persistence.MetaData;
import net.meisen.general.genmisc.exceptions.registry.IExceptionRegistry;

/**
 * Mock of {@code BasePersistor}.
 * 
 * @author pmeisen
 * 
 */
public class MockPersistor extends BasePersistor {

	/**
	 * Constructor specifying the {@code exceptionRegistry} to be used.
	 * 
	 * @param exceptionRegistry
	 *            the {@code exceptionRegistry} to be used
	 */
	public MockPersistor(IExceptionRegistry exceptionRegistry) {
		super(exceptionRegistry);
	}

	@Override
	public void save(final ILocation location, final MetaData... additionalData) {
		// empty
	}

	@Override
	public void load(final ILocation location, final MetaData... additionalData) {
		// empty
	}

	@Override
	protected OutputStream _openForWrite(Identifier identifier) {
		return null;
	}

	@Override
	public void close(Identifier identifier) {
		// empty
	}
}
