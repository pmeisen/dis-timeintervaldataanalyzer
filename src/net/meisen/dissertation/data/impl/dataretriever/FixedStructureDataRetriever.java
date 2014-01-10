package net.meisen.dissertation.data.impl.dataretriever;

import net.meisen.dissertation.exceptions.DataRetrieverException;
import net.meisen.dissertation.models.impl.dataretriever.BaseDataRetriever;
import net.meisen.dissertation.models.impl.dataretriever.IDataRetrieverConfig;
import net.meisen.dissertation.models.impl.dataretriever.IQueryConfiguration;

public class FixedStructureDataRetriever extends BaseDataRetriever {

	/**
	 * Constructor used to create a new instance of a
	 * {@code FixedStructureDataRetriever}.
	 * 
	 * @param config
	 *            the configuration to be used for the
	 *            {@code FixedStructureDataRetriever}
	 */
	public FixedStructureDataRetriever(final IDataRetrieverConfig config) {
		super(config);
	}

	@Override
	protected Class<? extends IDataRetrieverConfig> supportedConfiguration() {
		return FixedStructureDataRetrieverConfig.class;
	}

	@Override
	protected FixedStructureDataRetrieverConfig createDefaultConfig() {

		// create a configuration with just one random integer value
		final FixedStructureDataRetrieverConfig defConfig = new FixedStructureDataRetrieverConfig();
		final FixedStructureDataRetrieverConfigEntry entry = new FixedStructureDataRetrieverConfigEntry();
		entry.setName("RANDOM_INTEGER");
		entry.setType(Integer.class);
		defConfig.addEntry(entry);

		return defConfig;
	}

	@Override
	public FixedStructureDataCollection retrieve(
			final IQueryConfiguration queryConfiguration) {

		if (queryConfiguration == null) {
			// do nothing everything is fine
		} else if (queryConfiguration instanceof FixedStructureQueryConfig) {
			// do nothing everything is fine
		} else {
			exceptionRegistry.throwRuntimeException(
					DataRetrieverException.class, 1002,
					FixedStructureDataRetriever.class.getName(),
					queryConfiguration.getClass().getName(),
					FixedStructureQueryConfig.class.getName());
		}

		// get the values needed
		final FixedStructureDataRetrieverConfig config = getConfig();
		final FixedStructureQueryConfig query = (FixedStructureQueryConfig) queryConfiguration;

		// create the collection
		return new FixedStructureDataCollection(config, query);
	}
}
