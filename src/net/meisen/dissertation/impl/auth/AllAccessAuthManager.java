package net.meisen.dissertation.impl.auth;

import java.util.Collections;
import java.util.HashSet;
import java.util.Set;

import net.meisen.dissertation.exceptions.AuthException;
import net.meisen.dissertation.exceptions.AuthManagementException;
import net.meisen.dissertation.model.auth.IAuthManager;
import net.meisen.dissertation.model.auth.IAuthManagerConfig;
import net.meisen.dissertation.model.auth.permissions.DefinedPermission;
import net.meisen.dissertation.model.auth.permissions.Permission;
import net.meisen.dissertation.server.sessions.Session;

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

	private ThreadLocal<String> username = new ThreadLocal<String>();

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
		final String curUser = this.username.get();
		if (curUser != null && !curUser.equalsIgnoreCase(username)) {
			if (LOG.isWarnEnabled()) {
				LOG.warn("Trying to login multiple times: " + curUser + " vs. "
						+ username);
			}
		}

		this.username.set(username);
	}

	@Override
	public void logout() {
		this.username.set(null);
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

	@Override
	public Set<String> getUsers() {
		return Collections.<String> emptySet();
	}

	@Override
	public Set<String> getRoles() {
		return Collections.<String> emptySet();
	}

	@Override
	public Set<String> getUserRoles(final String username) {
		return Collections.<String> emptySet();
	}

	@Override
	public Set<DefinedPermission> getUserPermissions(final String username) {
		final Set<DefinedPermission> perms = new HashSet<DefinedPermission>();

		// add all available permission, use a wild-char for the models
		for (final Permission perm : Permission.values()) {
			if (perm.isGlobal()) {
				perms.add(perm.create());
			} else {
				perms.add(perm.create("*"));
			}
		}

		return perms;
	}

	@Override
	public Set<DefinedPermission> getAssignedUserPermissions(
			final String username) {
		return getUserPermissions(username);
	}

	@Override
	public Set<DefinedPermission> getRolePermissions(final String role) {
		final Set<DefinedPermission> perms = new HashSet<DefinedPermission>();

		// add all available permission, use a wild-char for the models
		for (final Permission perm : Permission.values()) {
			if (perm.isGlobal()) {
				perms.add(perm.create());
			} else {
				perms.add(perm.create("*"));
			}
		}

		return perms;
	}

	@Override
	public String getCurrentUsername() {
		return this.username.get();
	}

	@Override
	public void bind(final Session session) throws AuthException {
		// nothing to do
	}

	@Override
	public void unbind() throws AuthException {
		// nothing to do
	}
}
