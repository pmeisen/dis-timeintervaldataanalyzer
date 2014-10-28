package net.meisen.dissertation.impl.parser.query.unload;

import java.util.HashMap;
import java.util.Map;
import java.util.Map.Entry;

import net.meisen.dissertation.jdbc.protocol.QueryType;
import net.meisen.dissertation.model.auth.IAuthManager;
import net.meisen.dissertation.model.auth.permissions.DefinedPermission;
import net.meisen.dissertation.model.auth.permissions.Permission;
import net.meisen.dissertation.model.data.TidaModel;
import net.meisen.dissertation.model.handler.TidaModelHandler;
import net.meisen.dissertation.model.parser.query.IQuery;
import net.meisen.dissertation.model.parser.query.IQueryResult;
import net.meisen.dissertation.model.parser.query.IResourceResolver;
import net.meisen.general.genmisc.exceptions.ForwardedRuntimeException;

/**
 * Query to unload a specific {@code TidaModel}.
 * 
 * @author pmeisen
 * 
 */
public class UnloadQuery implements IQuery {
	private String modelId;

	private Map<String, Object> properties = new HashMap<String, Object>();

	@Override
	public boolean expectsModel() {
		return false;
	}

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
			final IResourceResolver resolver) throws ForwardedRuntimeException {

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

		// unload the model
		handler.unload(modelId);

		return new UnloadResult();
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
		return new DefinedPermission[][] { new DefinedPermission[] { Permission.unload
				.create() } };
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
	public String toString() {
		return "UNLOAD " + modelId;
	}
}
