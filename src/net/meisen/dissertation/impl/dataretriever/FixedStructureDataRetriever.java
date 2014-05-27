package net.meisen.dissertation.impl.dataretriever;

import net.meisen.dissertation.exceptions.DataRetrieverException;
import net.meisen.dissertation.model.dataretriever.BaseDataRetriever;
import net.meisen.dissertation.model.dataretriever.IDataRetrieverConfig;
import net.meisen.dissertation.model.dataretriever.IQueryConfiguration;

/**
 * A {@code DataRetriever} to retrieve random or fixed values in a pre-defined
 * (configured) structure.
 * 
 * @author pmeisen
 * 
 */
public class FixedStructureDataRetriever extends BaseDataRetriever {
	/**
	 * The name of the field added when using the default configuration
	 */
	public final static String DEF_NAME = "RANDOM_INTEGER";

	/**
	 * Constructs a {@code FixedStructureDataRetriever} using the specified
	 * default configuration.
	 * 
	 * @param id
	 *            an identifier for the retriever
	 */
	public FixedStructureDataRetriever(final String id) {
		this(id, null);
	}

	/**
	 * Constructor used to create a new instance of a
	 * {@code FixedStructureDataRetriever}.
	 * 
	 * @param id
	 *            an identifier for the retriever
	 * @param config
	 *            the configuration to be used for the
	 *            {@code FixedStructureDataRetriever}
	 */
	public FixedStructureDataRetriever(final String id,
			final IDataRetrieverConfig config) {
		super(id, config);
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
		entry.setName(DEF_NAME);
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
