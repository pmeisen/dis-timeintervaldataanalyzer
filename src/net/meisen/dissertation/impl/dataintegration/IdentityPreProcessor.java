package net.meisen.dissertation.impl.dataintegration;

import net.meisen.dissertation.model.dataintegration.IPreProcessor;
import net.meisen.dissertation.model.dataintegration.IPreProcessorConfig;
import net.meisen.dissertation.model.datasets.IDataRecord;

/**
 * Simple pre-processor mapping the raw record to the new one (without copying).
 * 
 * @author pmeisen
 * 
 */
public class IdentityPreProcessor implements IPreProcessor {

	@Override
	public void setConfig(final IPreProcessorConfig config) {
		// nothing to do
	}

	@Override
	public IDataRecord process(final IDataRecord raw) {
		return raw;
	}
}
