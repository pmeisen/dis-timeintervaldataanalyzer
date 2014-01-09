package net.meisen.dissertation.models.impl.dataretriever;

import net.meisen.dissertation.exceptions.DataRetrieverException;
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
	@Qualifier("exceptionRegistry")
	protected IExceptionRegistry exceptionRegistry;

	/**
	 * Constructor with the {@code DataRetrieverConfiguration}, the
	 * {@code config} must be of a valid type.
	 * 
	 * @param config
	 *            the {@code DataRetrieverConfiguration} used for the
	 *            {@code DataRetriever}
	 * 
	 * @see #supportedConfiguration()
	 */
	public BaseDataRetriever(final IDataRetrieverConfig config) {
		final Class<? extends IDataRetrieverConfig> configClazz = supportedConfiguration();
		if (config == null && needConfiguration()) {
			exceptionRegistry.throwException(DataRetrieverException.class,
					1000, getClass().getName());
		} else if (configClazz != null
				&& !configClazz.isAssignableFrom(config.getClass())) {
			exceptionRegistry.throwException(DataRetrieverException.class,
					1001, getClass().getName(), configClazz.getName(), config
							.getClass().getName());
		}

		this.config = config;
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
	 * constructor. By default the type has to be
	 * {@link IDataRetrieverConfig}.
	 * 
	 * @return the type of the configuration which has to be passed to the
	 *         constructor
	 */
	protected Class<? extends IDataRetrieverConfig> supportedConfiguration() {
		return IDataRetrieverConfig.class;
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
}
