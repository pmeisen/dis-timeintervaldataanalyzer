package net.meisen.dissertation.models.impl.dataretriever;

import net.meisen.dissertation.exceptions.DataRetrieverException;
import net.meisen.general.genmisc.exceptions.registry.IExceptionRegistry;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;

public abstract class BaseDataRetriever {
	private IDataRetrieverConfiguration config;

	@Autowired
	@Qualifier("exceptionRegistry")
	protected IExceptionRegistry exceptionRegistry;

	public BaseDataRetriever(final IDataRetrieverConfiguration config) {
		final Class<? extends IDataRetrieverConfiguration> configClazz = supportedConfiguration();
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

	protected boolean needConfiguration() {
		return false;
	}

	@SuppressWarnings("unchecked")
	protected <T extends IDataRetrieverConfiguration> T getConfig() {
		return (T) config;
	}

	protected Class<? extends IDataRetrieverConfiguration> supportedConfiguration() {
		return IDataRetrieverConfiguration.class;
	}

	public abstract DataCollection<?> retrieve(
			final IQueryConfiguration queryConfiguration);
}
