package net.meisen.dissertation.impl.parser.query.remove;

import java.util.LinkedHashSet;
import java.util.List;
import java.util.Set;

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
 * A query to remove roles from a user.
 * 
 * @author pmeisen
 * 
 */
public class RemoveQuery implements IQuery {
	private final Set<String> roles = new LinkedHashSet<String>();

	private String entityName;

	/**
	 * Gets the roles defined for the entity to be removed.
	 * 
	 * @return the roles defined for the entity to be removed
	 */
	public Set<String> getRoles() {
		return roles;
	}

	/**
	 * Sets the roles defined for the entity to be removed.
	 * 
	 * @param roles
	 *            the roles defined for the entity to be removed
	 */
	public void setRoles(final List<String> roles) {
		this.roles.clear();
		this.roles.addAll(roles);
	}

	/**
	 * Adds the roles for the entity to be removed to the one defined so far.
	 * 
	 * @param roles
	 *            the roles for the entity to be removed to the one defined so
	 *            far
	 */
	public void addRoles(final List<String> roles) {
		this.roles.addAll(roles);
	}

	/**
	 * Sets the name of the entity to remove the roles from.
	 * 
	 * @param entityName
	 *            the name of the entity to remove the roles from
	 */
	public void setEntityName(final String entityName) {
		this.entityName = entityName;
	}

	/**
	 * Gets the name of the entity to remove the roles from.
	 * 
	 * @return the name of the entity to remove the roles from
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
	public RemoveResult evaluate(final IAuthManager authManager,
			final TidaModelHandler handler, final TidaModel model,
			final IResourceResolver resolver) throws ForwardedRuntimeException,
			CancellationException {

		final String entityName = getEntityName();
		for (final String role : roles) {
			authManager.removeRoleFromUser(entityName, role);
		}

		return new RemoveResult();
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
		return "REMOVE FROM " + entityName + " (roles: " + getRoles() + ")";
	}
}
