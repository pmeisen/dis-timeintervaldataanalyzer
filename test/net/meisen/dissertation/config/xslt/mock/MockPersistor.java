package net.meisen.dissertation.config.xslt.mock;

import java.io.OutputStream;

import net.meisen.dissertation.model.persistence.BasePersistor;
import net.meisen.dissertation.model.persistence.Identifier;

/**
 * Mock of {@code BasePersistor}.
 * 
 * @author pmeisen
 * 
 */
public class MockPersistor extends BasePersistor {

	@Override
	public void save(String location) {
		// empty
	}

	@Override
	public void load(String location) {
		// empty
	}

	@Override
	public OutputStream openForWrite(Identifier identifier) {
		return null;
	}

	@Override
	public void close(Identifier identifier) {
		// empty
	}
}
