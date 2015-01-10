package net.meisen.dissertation.impl.parser.query.modify;

import net.meisen.dissertation.jdbc.protocol.QueryType;
import net.meisen.dissertation.model.auth.IAuthManager;
import net.meisen.dissertation.model.auth.permissions.DefinedPermission;
import net.meisen.dissertation.model.auth.permissions.Permission;
import net.meisen.dissertation.model.data.TidaModel;
import net.meisen.dissertation.model.handler.TidaModelHandler;
import net.meisen.dissertation.model.parser.query.IQuery;
import net.meisen.dissertation.model.parser.query.IResourceResolver;
import net.meisen.dissertation.server.CancellationException;
import net.meisen.general.genmisc.exceptions.ForwardedRuntimeException;

/**
 * Query to modify attributes of an entity.
 * 
 * @author pmeisen
 * 
 */
public class ModifyQuery implements IQuery {
	private String modelId;
	private String entityName;
	private String entityValue;
	private ModifyType type;

	/**
	 * Sets the name of the entity to be modified.
	 * 
	 * @param entityName
	 *            the name of the entity to be modified
	 */
	public void setEntityName(final String entityName) {
		this.entityName = entityName;
	}

	/**
	 * Gets the name of the entity to be modified.
	 * 
	 * @return the name of the entity to be modified
	 */
	public String getEntityName() {
		return entityName;
	}

	/**
	 * Gets the value specified for the entity to be modified.
	 * 
	 * @return the value specified for the entity to be modified
	 */
	public String getEntityValue() {
		return entityValue;
	}

	/**
	 * Sets the value for the entity to be modified.
	 * 
	 * @param entityValue
	 *            the new value of the entity
	 */
	public void setEntityValue(final String entityValue) {
		this.entityValue = entityValue;
	}

	@Override
	public boolean expectsModel() {
		return ModifyType.MODEL.equals(getType());
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
	public ModifyResult evaluate(final IAuthManager authManager,
			final TidaModelHandler handler, final TidaModel model,
			final IResourceResolver resolver) throws ForwardedRuntimeException,
			CancellationException {

		if (ModifyType.MODEL.equals(getType())) {
			model.setBulkLoad(getEntityValue().equalsIgnoreCase("true"));
		} else {
			authManager.modifyPassword(getEntityName(), getEntityValue());
		}

		return new ModifyResult();
	}

	@Override
	public QueryType getQueryType() {
		return QueryType.MANIPULATION;
	}

	@Override
	public void enableIdCollection(final boolean enableIdCollection) {
		// nothing to do cannot be enabled
	}

	@Override
	public DefinedPermission[][] getNeededPermissions() {
		if (ModifyType.MODEL.equals(getType())) {
			return new DefinedPermission[][] {
					new DefinedPermission[] { Permission.modify.create(modelId) },
					new DefinedPermission[] { Permission.modifyAll.create() } };
		} else {
			return new DefinedPermission[][] { new DefinedPermission[] { Permission.manageUsers
					.create() } };
		}
	}

	@Override
	public String toString() {
		return "MODIFY " + type + " " + entityName + " VALUE " + entityValue;
	}

	/**
	 * Gets the type of the modification.
	 * 
	 * @return the type of the modification
	 */
	public ModifyType getType() {
		return type;
	}

	/**
	 * Sets the type of the modification
	 * 
	 * @param type
	 *            the type of the modification
	 */
	public void setType(final ModifyType type) {
		this.type = type;
	}
}
