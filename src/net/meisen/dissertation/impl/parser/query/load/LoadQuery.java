package net.meisen.dissertation.impl.parser.query.load;

import java.io.InputStream;
import java.util.HashMap;
import java.util.Map;
import java.util.Map.Entry;

import net.meisen.dissertation.exceptions.QueryEvaluationException;
import net.meisen.dissertation.exceptions.TidaModelHandlerException;
import net.meisen.dissertation.jdbc.protocol.QueryType;
import net.meisen.dissertation.model.auth.IAuthManager;
import net.meisen.dissertation.model.auth.permissions.DefinedPermission;
import net.meisen.dissertation.model.auth.permissions.Permission;
import net.meisen.dissertation.model.data.TidaModel;
import net.meisen.dissertation.model.handler.TidaModelHandler;
import net.meisen.dissertation.model.parser.query.IQuery;
import net.meisen.dissertation.model.parser.query.IQueryResult;
import net.meisen.dissertation.model.parser.query.IResourceResolver;
import net.meisen.dissertation.server.CancellationException;
import net.meisen.general.genmisc.exceptions.ForwardedRuntimeException;
import net.meisen.general.genmisc.types.Streams;

/**
 * A query used to load a {@code TidaModel}.
 * 
 * @author pmeisen
 * 
 */
public class LoadQuery implements IQuery {

	private String modelId;
	private String path;

	private Map<String, Object> properties = new HashMap<String, Object>();

	@Override
	public String getModelId() {
		return modelId;
	}

	@Override
	public void setModelId(final String modelId) {
		this.modelId = modelId;
	}

	@Override
	public IQueryResult evaluate(final IAuthManager authManager,
			final TidaModelHandler handler, final TidaModel model,
			final IResourceResolver resolver) throws CancellationException {

		final String modelId;
		if (getPath() == null) {
			modelId = getModelId();

			// check if the model is already loaded
			if (handler.getTidaModel(modelId) != null) {
				final boolean force = getProperty("force", true);
				if (force) {
					throw new ForwardedRuntimeException(
							QueryEvaluationException.class, 1013, modelId);
				}
			}
			// let's load the model otherwise
			else {
				try {
					handler.loadFromDefaultLocation(modelId);
				} catch (final TidaModelHandlerException e) {
					throw new ForwardedRuntimeException(
							QueryEvaluationException.class, 1012, e, modelId);
				}
			}
		} else if (resolver == null) {
			throw new ForwardedRuntimeException(QueryEvaluationException.class,
					1014, getPath());
		} else {
			final boolean force = getProperty("force", true);
			final InputStream is = resolver.resolve(getPath());

			final TidaModel loadedModel;
			try {
				loadedModel = handler.loadViaXslt(is, force);
			} catch (final RuntimeException e) {
				throw e;
			} finally {

				// make sure the resource is closed again
				Streams.closeIO(is);
			}

			modelId = loadedModel.getId();
		}

		// check if the model should be registered for auto-load
		final Boolean autoload = getProperty("autoload", null);

		// do not change the setting
		if (autoload == null) {
			// do nothing
		}
		// autoload should be enabled
		else if (autoload) {
			handler.enableAutoload(modelId);
		}
		// autoload should be disabled
		else {
			handler.disableAutoload(modelId);
		}

		// create the result, the loading is finished
		final LoadResult result = new LoadResult();
		result.setModelId(modelId);

		return result;
	}

	/**
	 * Gets the specified path (of the client) to load the model from.
	 * 
	 * @return the path to load the model from (on client-side)
	 */
	public String getPath() {
		return path;
	}

	/**
	 * Sets the specified path (of the client) to load the model from.
	 * 
	 * @param path
	 *            the path to load the model from (on client-side)
	 */
	public void setPath(final String path) {
		this.path = path;
	}

	@Override
	public boolean expectsModel() {
		return false;
	}

	@Override
	public String toString() {
		return "load " + getModelId()
				+ (getPath() == null ? "" : " from " + getPath());
	}

	/**
	 * Sets the specified {@code properties} of {@code this}.
	 * 
	 * @param properties
	 *            the properties to be set
	 */
	public void setProperties(final Map<String, Object> properties) {
		for (final Entry<String, Object> e : properties.entrySet()) {
			this.properties.put(e.getKey().toUpperCase(), e.getValue());
		}
	}

	/**
	 * Reads the value of the specified {@code property}.
	 * 
	 * @param property
	 *            the property to retrieve the value for
	 * @param defaultValue
	 *            the default value to be used if the property is not set
	 * 
	 * @return the determined value
	 */
	@SuppressWarnings("unchecked")
	public <T> T getProperty(final String property, final T defaultValue) {
		final Object value = properties.get(property.toUpperCase());

		if (value == null) {
			return defaultValue;
		} else {
			return (T) value;
		}
	}

	@Override
	public QueryType getQueryType() {
		return QueryType.MANIPULATION;
	}

	@Override
	public void enableIdCollection(final boolean enableIdCollection) {
		// ignore not supported
	}

	@Override
	public DefinedPermission[][] getNeededPermissions() {
		return new DefinedPermission[][] { new DefinedPermission[] { Permission.load
				.create() } };
	}
}
