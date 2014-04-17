package net.meisen.dissertation.impl.time.mapper;

import net.meisen.dissertation.exceptions.MapperFactoryException;
import net.meisen.dissertation.model.time.mapper.BaseMapper;
import net.meisen.dissertation.model.time.mapper.BaseMapperFactory;
import net.meisen.dissertation.model.time.mapper.IMapperFactoryConfig;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * The default implementation of a {@code BaseMapperFactory} which registers the
 * default {@code Mappers}, i.e.:
 * <ul>
 * <li>{@code DateMapper}</li>
 * <li>{@code LongMapper}</li>
 * </ul>
 * 
 * @author pmeisen
 * 
 * @see DateMapper
 * @see LongMapper
 * 
 */
public class MapperFactory extends BaseMapperFactory {
	private final static Logger LOG = LoggerFactory
			.getLogger(MapperFactory.class);

	private MapperFactoryConfig config;

	@Override
	public void setConfig(final IMapperFactoryConfig config) {

		// check if we have a configuration already
		if (this.config != null) {
			if (LOG.isWarnEnabled()) {
				LOG.warn("The configuration of the factory is already defined, but changed.");
			}
		}

		if (config == null) {
			this.config = new MapperFactoryConfig();
		} else if (config instanceof MapperFactoryConfig == false) {
			if (exceptionRegistry == null) {
				throw new IllegalArgumentException(
						"The configuration used for '"
								+ getClass().getSimpleName()
								+ "' must be of the type '"
								+ MapperFactoryConfig.class.getName() + "'.");
			} else {
				exceptionRegistry.throwException(MapperFactoryException.class,
						1000, getClass().getSimpleName(),
						MapperFactoryConfig.class.getName());
			}
		} else {
			this.config = (MapperFactoryConfig) config;
		}

		if (LOG.isDebugEnabled()) {
			LOG.debug("Setting the '" + getClass().getName()
					+ "' with configuration " + config);
		}

		// apply the configuration
		removeAllMapper();
		for (final Class<? extends BaseMapper<?>> mapperClazz : this.config
				.getMapper()) {
			addMapper(mapperClazz);
		}
	}

	/**
	 * Gets the configuration used for the {@code MapperFactory}.
	 * 
	 * @return the configuration used
	 */
	public IMapperFactoryConfig getConfig() {
		return getConfiguration();
	}

	/**
	 * Gets the specified configuration. A default configuration is created, if
	 * none is defined so far. Therefore this method will never return
	 * {@code null}.
	 * 
	 * @return the configuration used by the {@code IndexFactory}
	 */
	public MapperFactoryConfig getConfiguration() {
		if (config == null) {
			this.config = new MapperFactoryConfig();

			if (LOG.isInfoEnabled()) {
				LOG.info("Using default configuration " + this.config);
			}
		}

		return this.config;
	}
}
