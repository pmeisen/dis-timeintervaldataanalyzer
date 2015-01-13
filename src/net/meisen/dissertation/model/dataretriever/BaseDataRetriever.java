package net.meisen.dissertation.model.dataretriever;

import net.meisen.dissertation.config.xslt.DefaultValues;
import net.meisen.dissertation.exceptions.DataRetrieverException;
import net.meisen.general.genmisc.exceptions.ForwardedRuntimeException;
import net.meisen.general.genmisc.exceptions.registry.IExceptionRegistry;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;

/**
 * A base implementation of a {@code DataRetriever}.
 * 
 * @author pmeisen
 * 
 */
public abstract class BaseDataRetriever {
	private IDataRetrieverConfig config;

	/**
	 * The {@code ExceptionRegistry} used to throw exceptions.
	 */
	@Autowired
	@Qualifier(DefaultValues.EXCEPTIONREGISTRY_ID)
	protected IExceptionRegistry exceptionRegistry;

	private final String id;

	/**
	 * Constructor with the {@code DataRetrieverConfiguration}, the
	 * {@code config} must be of a valid type.
	 * 
	 * @param id
	 *            an identifier for the retriever
	 * @param config
	 *            the {@code DataRetrieverConfiguration} used for the
	 *            {@code DataRetriever}
	 * 
	 * @see #supportedConfiguration()
	 */
	public BaseDataRetriever(final String id, final IDataRetrieverConfig config) {
		this.id = id;

		final Class<? extends IDataRetrieverConfig> configClazz = supportedConfiguration();
		if (config == null && needConfiguration()) {
			throw new ForwardedRuntimeException(DataRetrieverException.class,
					1000, getClass().getName());
		} else if (config == null) {
			this.config = createDefaultConfig();
		} else if (configClazz != null
				&& !configClazz.isAssignableFrom(config.getClass())) {
			throw new ForwardedRuntimeException(DataRetrieverException.class,
					1001, getClass().getName(), configClazz.getName(), config
							.getClass().getName());
		} else {
			this.config = config;
		}
	}

	/**
	 * Defines if a configuration is needed by the {@code DataRetriever}. The
	 * default implementation returns {@code false}, which indicates that the
	 * retriever doesn't need a configuration. If one is needed this method
	 * should be overridden and return {@code true}.
	 * 
	 * @return {@code true} if a configuration has to be passed to the
	 *         constructor, otherwise {@code null} is allowed
	 */
	protected boolean needConfiguration() {
		return false;
	}

	/**
	 * Gets the configuration defined for the {@code DataRertriever}. This
	 * method can only return {@code null} if the {@link #needConfiguration()}
	 * returns {@code false}, otherwise the method returns always a
	 * configuration. The type of the configuration can also be ensured using
	 * the {@link #supportedConfiguration()}.
	 * 
	 * @return the defined configuration
	 */
	@SuppressWarnings("unchecked")
	protected <T extends IDataRetrieverConfig> T getConfig() {
		return (T) config;
	}

	/**
	 * Defines the type of the configuration which has to be passed to the
	 * constructor. By default the type has to be {@link IDataRetrieverConfig}.
	 * 
	 * @return the type of the configuration which has to be passed to the
	 *         constructor
	 */
	protected Class<? extends IDataRetrieverConfig> supportedConfiguration() {
		return IDataRetrieverConfig.class;
	}

	/**
	 * This method is called if the {@code DataRetriever} allows to run without
	 * any configuration (i.e. {@link #needConfiguration()} returns
	 * {@code false}). It is used to create and assign a default configuration
	 * in the case, that no configuration is passed. The default implementation
	 * just returns {@code null}.
	 * 
	 * @return a new instance of a default {@code DataRetrieverConfig} for the
	 *         {@code DataRetriever}
	 */
	protected IDataRetrieverConfig createDefaultConfig() {
		return null;
	}

	/**
	 * Method to retrieve the data for the specified {@code queryConfiguration}.
	 * 
	 * @param queryConfiguration
	 *            the configuration which specifies which data to retrieve
	 * @return the retrieved {@code DataCollection}
	 */
	public abstract DataCollection<?> retrieve(
			final IQueryConfiguration queryConfiguration);

	/**
	 * The release method is invoked whenever the {@code DataRetriever} should
	 * release all the resources used or blocked by the retriever. The base
	 * implementation does nothing and should be overridden when needed.
	 */
	public void release() {
		// do nothing
	}

	/**
	 * Gets the identifier of the {@code BaseDataRetriever}.
	 * 
	 * @return the identifier of {@code this}
	 */
	public String getId() {
		return id;
	}

	/**
	 * Sets the exception registry to be used. The registry is auto-wired, but
	 * anyways sometimes thats not a solution.
	 * 
	 * @param exceptionRegistry
	 *            the exception registry to be used
	 */
	public void setExceptionRegistry(final IExceptionRegistry exceptionRegistry) {
		this.exceptionRegistry = exceptionRegistry;
	}
}
