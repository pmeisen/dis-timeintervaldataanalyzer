package net.meisen.dissertation.impl.parser.query.add;

import java.util.Collection;
import java.util.LinkedHashSet;
import java.util.List;
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
 * A query used to add users or roles.
 * 
 * @author pmeisen
 * 
 */
public class AddQuery implements IQuery {
	private final Set<String> roles = new LinkedHashSet<String>();
	private final Set<DefinedPermission> permissions = new LinkedHashSet<DefinedPermission>();

	private AddType entityType;
	private String entityName;
	private String entityPassword;

	/**
	 * Gets the type of the entity to be added.
	 * 
	 * @return the type of the entity to be added
	 */
	public AddType getEntityType() {
		return entityType;
	}

	/**
	 * Sets the type of the entity to be added.
	 * 
	 * @param type
	 *            the type of the entity to be added
	 */
	public void setEntityType(final AddType type) {
		this.entityType = type;
	}

	/**
	 * Gets the roles defined for the entity to be added.
	 * 
	 * @return the roles defined for the entity to be added
	 */
	public Set<String> getRoles() {
		return roles;
	}

	/**
	 * Sets the roles defined for the entity to be added.
	 * 
	 * @param roles
	 *            the roles defined for the entity to be added
	 */
	public void setRoles(final List<String> roles) {
		this.roles.clear();
		this.roles.addAll(roles);
	}

	/**
	 * Adds the roles for the entity to be added to the one defined so far.
	 * 
	 * @param roles
	 *            the roles for the entity to be added to the one defined so far
	 */
	public void addRoles(final List<String> roles) {
		this.roles.addAll(roles);
	}

	/**
	 * Gets the permissions defined for the entity to be added.
	 * 
	 * @return the permissions defined for the entity to be added
	 */
	public Set<DefinedPermission> getPermissions() {
		return permissions;
	}

	/**
	 * Sets the permissions to be granted for the entity.
	 * 
	 * @param permissions
	 *            the permissions to be granted for the entity
	 */
	public void setPermissions(final Collection<DefinedPermission> permissions) {
		this.permissions.clear();
		this.permissions.addAll(permissions);
	}

	/**
	 * Adds the permissions to the already defined once, which should be granted
	 * to the entity to be added.
	 * 
	 * @param permissions
	 *            the permissions, which should be added to the already defined
	 *            once, to be granted to the entity
	 */
	public void addPermissions(final Collection<DefinedPermission> permissions) {
		this.permissions.addAll(permissions);
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

	/**
	 * Gets the password specified for the entity to be added.
	 * 
	 * @return the password specified for the entity to be added
	 */
	public String getEntityPassword() {
		return entityPassword;
	}

	/**
	 * Sets the password for the entity to be added.
	 * 
	 * @param entityPassword
	 *            the password for the entity to be added
	 */
	public void setEntityPassword(String entityPassword) {
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
		// nothing to do
	}

	@Override
	public AddResult evaluate(final IAuthManager authManager,
			final TidaModelHandler handler, final TidaModel model,
			final IResourceResolver resolver) throws ForwardedRuntimeException,
			CancellationException {

		if (AddType.USER.equals(getEntityType())) {
			final String[] roles = getRoles().toArray(new String[0]);
			final DefinedPermission[] perms = getPermissions().toArray(
					new DefinedPermission[0]);

			authManager.addUser(getEntityName(), getEntityPassword(), roles,
					perms);
		} else if (AddType.ROLE.equals(getEntityType())) {
			final DefinedPermission[] perms = getPermissions().toArray(
					new DefinedPermission[0]);

			authManager.addRole(getEntityName(), perms);
		} else {
			throw new ForwardedRuntimeException(QueryEvaluationException.class,
					1020, getEntityType());
		}

		return new AddResult();
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
		return entityType + " " + entityName + " (permissions: "
				+ getPermissions() + ", roles: " + getRoles() + ")";
	}
}
