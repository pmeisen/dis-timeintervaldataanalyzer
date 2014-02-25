package net.meisen.dissertation.config.xslt.mock;

import java.io.OutputStream;

import net.meisen.dissertation.model.persistence.BasePersistor;
import net.meisen.dissertation.model.persistence.Identifier;
import net.meisen.dissertation.model.persistence.ILocation;
import net.meisen.dissertation.model.persistence.MetaData;

/**
 * Mock of {@code BasePersistor}.
 * 
 * @author pmeisen
 * 
 */
public class MockPersistor extends BasePersistor {

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