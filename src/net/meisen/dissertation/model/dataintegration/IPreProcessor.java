package net.meisen.dissertation.model.dataintegration;

import net.meisen.dissertation.model.datasets.IDataRecord;

/**
 * A {@code PreProcessor} is used to process a record prior to adding it to the
 * system.
 * 
 * @author pmeisen
 * 
 */
public interface IPreProcessor {

	/**
	 * Method called prior to any processing and applying the configuration.
	 * 
	 * @param config
	 *            the configuration of the processor
	 */
	public void setConfig(final IPreProcessorConfig config);

	/**
	 * Method used to pre-process the specified record. The processing should
	 * not modify the {@code raw} record. Instead a new instance should be
	 * created.
	 * 
	 * @param raw
	 *            the raw record to be processed
	 * 
	 * @return the processed record
	 */
	public IDataRecord process(final IDataRecord raw);
}
