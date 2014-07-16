package net.meisen.dissertation.impl.auth;

import net.meisen.dissertation.exceptions.AuthManagementException;
import net.meisen.dissertation.model.auth.IAuthManager;
import net.meisen.dissertation.model.auth.IAuthManagerConfig;
import net.meisen.dissertation.model.auth.permissions.DefinedPermission;
import net.meisen.dissertation.model.auth.permissions.Permission;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * A {@code AuthManager} which grantes all {@code Permissions}.
 * 
 * @author pmeisen
 * 
 * @see Permission
 */
public class AllAccessAuthManager implements IAuthManager {
	private final static Logger LOG = LoggerFactory
			.getLogger(AllAccessAuthManager.class);

	@Override
	public void init() {
		if (LOG.isTraceEnabled()) {
			LOG.trace("Initializing the '" + getClass().getSimpleName()
					+ "' using the default settings.");
		}
	}

	@Override
	public void release() {
		if (LOG.isTraceEnabled()) {
			LOG.trace("Destroying the '" + getClass().getSimpleName() + "'.");
		}
	}

	@Override
	public void setConfig(final IAuthManagerConfig config) {
		// nothing can be configured
	}

	@Override
	public boolean hasPermission(final DefinedPermission permission) {
		return true;
	}

	@Override
	public void login(final String username, final String password) {
		// do nothing everything is available anyways
	}

	@Override
	public void logout() {
		// do nothing everything is available anyways
	}

	@Override
	public boolean isManageable() {
		return false;
	}

	@Override
	public void addUser(final String name, final String password,
			final String[] roles, final DefinedPermission[] permissions) {
		// nothing to be done, it's not manageable
	}

	@Override
	public void deleteUser(final String name) {
		// nothing to be done, it's not manageable
	}

	@Override
	public void modifyPassword(final String name, final String password) {
		// nothing to be done, it's not manageable
	}

	@Override
	public void addRole(final String role, final DefinedPermission[] permissions) {
		// nothing to be done, it's not manageable
	}

	@Override
	public void deleteRole(final String role) {
		// nothing to be done, it's not manageable
	}

	@Override
	public void assignRoleToUser(final String username, final String role) {
		// nothing to be done, it's not manageable
	}

	@Override
	public void removeRoleFromUser(final String username, final String role) {
		// nothing to be done, it's not manageable
	}

	@Override
	public void grantPermissionsToUser(final String username,
			final DefinedPermission[] permissions) {
		// nothing to be done, it's not manageable
	}

	@Override
	public void revokePermissionsFromUser(final String username,
			final DefinedPermission[] permissions) {
		// nothing to be done, it's not manageable
	}

	@Override
	public void grantPermissionsToRole(final String role,
			final DefinedPermission[] permissions)
			throws AuthManagementException {
		// nothing to be done, it's not manageable
	}

	@Override
	public void revokePermissionsFromRole(final String role,
			final DefinedPermission[] permissions)
			throws AuthManagementException {
		// nothing to be done, it's not manageable
	}
}
