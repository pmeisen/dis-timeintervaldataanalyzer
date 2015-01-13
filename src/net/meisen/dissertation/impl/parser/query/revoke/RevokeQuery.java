package net.meisen.dissertation.impl.parser.query.revoke;

import java.util.Collection;
import java.util.LinkedHashSet;
import java.util.Set;

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
 * A query used to revoke permissions from an entity, i.e. user or role.
 * 
 * @author pmeisen
 * 
 */
public class RevokeQuery implements IQuery {
	private final Set<DefinedPermission> permissions = new LinkedHashSet<DefinedPermission>();

	private RevokeType entityType;
	private String entityName;

	/**
	 * Gets the type of the entity to revoke the permissions from.
	 * 
	 * @return the type of the entity to revoke the permissions from
	 */
	public RevokeType getEntityType() {
		return entityType;
	}

	/**
	 * Sets the type of the entity to revoke the permissions from.
	 * 
	 * @param type
	 *            the type of the entity to revoke the permissions from
	 */
	public void setEntityType(final RevokeType type) {
		this.entityType = type;
	}

	/**
	 * Gets the permissions defined for the entity to be revoked.
	 * 
	 * @return the permissions defined for the entity to be revoked
	 */
	public Set<DefinedPermission> getPermissions() {
		return permissions;
	}

	/**
	 * Sets the permissions to be revoked from the entity.
	 * 
	 * @param permissions
	 *            the permissions to be revoked from the entity
	 */
	public void setPermissions(final Collection<DefinedPermission> permissions) {
		this.permissions.clear();
		this.permissions.addAll(permissions);
	}

	/**
	 * Adds the permissions to the already defined once, which should be revoked
	 * from the entity.
	 * 
	 * @param permissions
	 *            the permissions, which should be added to the already defined
	 *            once, to be revoked from the entity
	 */
	public void addPermissions(final Collection<DefinedPermission> permissions) {
		this.permissions.addAll(permissions);
	}

	/**
	 * Sets the name of the entity to revoke the permissions from.
	 * 
	 * @param entityName
	 *            the name of the entity to revoke the permissions from
	 */
	public void setEntityName(final String entityName) {
		this.entityName = entityName;
	}

	/**
	 * Gets the name of the entity to revoke the permissions from.
	 * 
	 * @return the name of the entity to revoke the permissions from
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
		return null;
	}

	@Override
	public void setModelId(final String modelId) {
		// nothing to do
	}

	@Override
	public RevokeResult evaluate(final IAuthManager authManager,
			final TidaModelHandler handler, final TidaModel model,
			final IResourceResolver resolver) throws ForwardedRuntimeException,
			CancellationException {

		if (RevokeType.USER.equals(getEntityType())) {
			final DefinedPermission[] perms = getPermissions().toArray(
					new DefinedPermission[0]);

			authManager.revokePermissionsFromUser(getEntityName(), perms);
		} else if (RevokeType.ROLE.equals(getEntityType())) {
			final DefinedPermission[] perms = getPermissions().toArray(
					new DefinedPermission[0]);

			authManager.revokePermissionsFromRole(getEntityName(), perms);
		} else {
			throw new ForwardedRuntimeException(QueryEvaluationException.class,
					1020, getEntityType());
		}

		return new RevokeResult();
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
		return new DefinedPermission[][] { new DefinedPermission[] { Permission.grantPermissions
				.create() } };
	}

	@Override
	public String toString() {
		return "GRANT " + entityType + " " + entityName + " (permissions: "
				+ getPermissions() + ")";
	}
}
