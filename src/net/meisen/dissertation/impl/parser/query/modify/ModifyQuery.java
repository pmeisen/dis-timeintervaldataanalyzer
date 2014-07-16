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
	private String entityName;
	private String entityPassword;

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
	 * Gets the password specified for the entity to be modified.
	 * 
	 * @return the password specified for the entity to be modified
	 */
	public String getEntityPassword() {
		return entityPassword;
	}

	/**
	 * Sets the password for the entity to be modified.
	 * 
	 * @param entityPassword
	 *            the password for the entity to be modified
	 */
	public void setEntityPassword(final String entityPassword) {
		this.entityPassword = entityPassword;
	}

	@Override
	public boolean expectsModel() {
		return false;
	}

	@Override
	public String getModelId() {
		return null;
	}

	@Override
	public void setModelId(final String modelId) {
		// ignore
	}

	@Override
	public ModifyResult evaluate(final IAuthManager authManager,
			final TidaModelHandler handler, final TidaModel model,
			final IResourceResolver resolver) throws ForwardedRuntimeException,
			CancellationException {
		authManager.modifyPassword(getEntityName(), getEntityPassword());

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
		return new DefinedPermission[][] { new DefinedPermission[] { Permission.manageUsers
				.create() } };
	}

	@Override
	public String toString() {
		return "MODIFY " + entityName;
	}
}
