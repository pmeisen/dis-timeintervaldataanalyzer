package net.meisen.dissertation.impl.parser.query.drop;

import net.meisen.dissertation.exceptions.QueryEvaluationException;
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
 * A query used to add users or roles.
 * 
 * @author pmeisen
 * 
 */
public class DropQuery implements IQuery {
	private DropType entityType;
	private String entityName;
	private String modelId;

	/**
	 * Gets the type of the entity to be dropped.
	 * 
	 * @return the type of the entity to be dropped
	 */
	public DropType getEntityType() {
		return entityType;
	}

	/**
	 * Sets the type of the entity to be dropped.
	 * 
	 * @param type
	 *            the type of the entity to be dropped
	 */
	public void setEntityType(final DropType type) {
		this.entityType = type;
	}

	/**
	 * Sets the name of the entity to be added.
	 * 
	 * @param entityName
	 *            the name of the entity to be added
	 */
	public void setEntityName(final String entityName) {
		this.entityName = entityName;
	}

	/**
	 * Gets the name of the entity to be added.
	 * 
	 * @return the name of the entity to be added
	 */
	public String getEntityName() {
		return entityName;
	}

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
	public DropResult evaluate(final IAuthManager authManager,
			final TidaModelHandler handler, final TidaModel model,
			final IResourceResolver resolver) throws ForwardedRuntimeException,
			CancellationException {

		if (DropType.USER.equals(getEntityType())) {
			authManager.deleteUser(getEntityName());
		} else if (DropType.ROLE.equals(getEntityType())) {
			authManager.deleteRole(getEntityName());
		} else if (DropType.MODEL.equals(getEntityType())) {
			handler.deleteModel(getModelId());
		} else {
			throw new ForwardedRuntimeException(QueryEvaluationException.class,
					1020, getEntityType());
		}

		return new DropResult();
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
		return new DefinedPermission[][] { new DefinedPermission[] { Permission.manageUsers
				.create() } };
	}

	@Override
	public String toString() {
		return "DROP " + entityType + " " + entityName;
	}
}
