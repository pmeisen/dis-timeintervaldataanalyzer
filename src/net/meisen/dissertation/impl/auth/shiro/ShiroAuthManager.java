package net.meisen.dissertation.impl.auth.shiro;

import net.meisen.dissertation.config.xslt.DefaultValues;
import net.meisen.dissertation.exceptions.AuthException;
import net.meisen.dissertation.exceptions.AuthManagementException;
import net.meisen.dissertation.exceptions.PermissionException;
import net.meisen.dissertation.model.auth.IAuthManager;
import net.meisen.dissertation.model.auth.IAuthManagerConfig;
import net.meisen.dissertation.model.auth.permissions.DefinedPermission;
import net.meisen.dissertation.model.auth.permissions.Permission;
import net.meisen.dissertation.model.handler.TidaModelHandler;
import net.meisen.general.genmisc.exceptions.ForwardedRuntimeException;
import net.meisen.general.genmisc.exceptions.registry.IExceptionRegistry;

import org.apache.shiro.authc.AuthenticationException;
import org.apache.shiro.authc.IncorrectCredentialsException;
import org.apache.shiro.authc.LockedAccountException;
import org.apache.shiro.authc.UnknownAccountException;
import org.apache.shiro.authc.UsernamePasswordToken;
import org.apache.shiro.mgt.DefaultSecurityManager;
import org.apache.shiro.realm.Realm;
import org.apache.shiro.subject.Subject;
import org.apache.shiro.util.Initializable;
import org.apache.shiro.util.LifecycleUtils;
import org.apache.shiro.util.ThreadContext;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;

/**
 * A {@code AuthManager} based on Shiro.
 * 
 * @author pmeisen
 * 
 * @see IAuthManager
 * 
 */
public class ShiroAuthManager implements IAuthManager {
	private final static Logger LOG = LoggerFactory
			.getLogger(ShiroAuthManager.class);
	private final static String permissionSeparator = ":";

	@Autowired
	@Qualifier(DefaultValues.HANDLER_ID)
	private TidaModelHandler handler;

	@Autowired
	@Qualifier(DefaultValues.EXCEPTIONREGISTRY_ID)
	private IExceptionRegistry exceptionRegistry;

	private DefaultSecurityManager manager;

	@Override
	public void init() {

		if (LOG.isTraceEnabled()) {
			LOG.trace("Initializing the '" + getClass().getSimpleName()
					+ "' using the default settings.");
		}

		// use the default realm
		final MapDbAuthorizingRealm realm = new MapDbAuthorizingRealm(
				handler.getDefaultLocation(true));
		if (realm instanceof Initializable) {
			realm.init();
		}

		// create a default manager and set it
		final DefaultSecurityManager manager = new DefaultSecurityManager();
		manager.setRealm(realm);

		// get the manager and initialize it
		if (manager instanceof Initializable) {
			((Initializable) manager).init();
		}

		this.manager = manager;
	}

	/**
	 * Gets the managable realm of the manager.
	 * 
	 * @return the {@code MapDbAuthorizingRealm} used
	 */
	protected MapDbAuthorizingRealm getManageableRealm() {
		for (final Realm realm : manager.getRealms()) {
			if (realm instanceof MapDbAuthorizingRealm) {
				return (MapDbAuthorizingRealm) realm;
			}
		}

		// if there is no manageable-realm, which cannot happen
		return null;
	}

	@Override
	public void release() {
		if (LOG.isTraceEnabled()) {
			LOG.trace("Destroying the '" + getClass().getSimpleName() + "'.");
		}

		// make sure the subject is logged out
		logout();

		// destroy the shiro manager
		LifecycleUtils.destroy(manager);
	}

	@Override
	public void setConfig(final IAuthManagerConfig config) {
		// there is no configuration available
	}

	@Override
	public boolean hasPermission(final DefinedPermission permission) {
		final Subject currentUser = getSubject();

		if (currentUser.isAuthenticated()) {
			final String p = permission.toString(permissionSeparator);
			return currentUser.isPermitted(p);
		} else {
			return false;
		}
	}

	/**
	 * Gets the subject of the current thread. There can only be one subject
	 * logged in within a thread.
	 * 
	 * @return the current subject of the current thread
	 */
	protected Subject getSubject() {
		Subject subject = ThreadContext.getSubject();
		if (subject == null) {
			subject = (new Subject.Builder(manager)).buildSubject();
			ThreadContext.bind(manager);
			ThreadContext.bind(subject);
		}

		return subject;
	}

	@Override
	public void login(final String username, final String password)
			throws AuthException {
		final UsernamePasswordToken token = new UsernamePasswordToken(username,
				password);
		token.setRememberMe(true);

		final Subject currentUser = getSubject();
		if (currentUser.isAuthenticated()) {

			// there is a user logged in
			exceptionRegistry.throwRuntimeException(AuthException.class, 1003,
					currentUser.getPrincipal());
		} else {

			// perform a login
			try {
				currentUser.login(token);
			} catch (final UnknownAccountException uae) {
				exceptionRegistry.throwRuntimeException(AuthException.class,
						1000, username);
			} catch (final IncorrectCredentialsException ice) {
				exceptionRegistry.throwRuntimeException(AuthException.class,
						1000, username);
			} catch (final LockedAccountException lae) {
				exceptionRegistry.throwRuntimeException(AuthException.class,
						1001, username);
			} catch (final AuthenticationException ae) {
				exceptionRegistry.throwRuntimeException(AuthException.class,
						1000, username);
			}
		}
	}

	@Override
	public void logout() {
		final Subject currentUser = getSubject();
		currentUser.logout();

		ThreadContext.unbindSubject();
		ThreadContext.unbindSecurityManager();
		ThreadContext.remove();
	}

	@Override
	public boolean isManageable() {
		return getManageableRealm() != null;
	}

	@Override
	public void addUser(final String name, final String password,
			final String[] roles, final DefinedPermission[] permissions)
			throws AuthManagementException {
		try {
			final String[] perms = Permission.transform(permissions,
					permissionSeparator);
			getManageableRealm().addUser(name, password, roles, perms);
		} catch (final ForwardedRuntimeException e) {
			exceptionRegistry.throwRuntimeException(e);
		}
	}

	@Override
	public void deleteUser(final String name) {
		getManageableRealm().deleteUser(name);
	}

	@Override
	public void modifyPassword(final String name, final String password)
			throws AuthManagementException {
		try {
			getManageableRealm().modifyPassword(name, password);
		} catch (final ForwardedRuntimeException e) {
			exceptionRegistry.throwRuntimeException(e);
		}
	}

	@Override
	public void addRole(final String role, final DefinedPermission[] permissions)
			throws AuthManagementException {
		try {
			final String[] perms = Permission.transform(permissions,
					permissionSeparator);
			getManageableRealm().addRole(role, perms);
		} catch (final ForwardedRuntimeException e) {
			exceptionRegistry.throwRuntimeException(e);
		}
	}

	@Override
	public void deleteRole(final String role) throws AuthManagementException,
			PermissionException {
		try {
			getManageableRealm().deleteRole(role);
		} catch (final ForwardedRuntimeException e) {
			exceptionRegistry.throwRuntimeException(e);
		}
	}

	@Override
	public void assignRoleToUser(final String username, final String role)
			throws AuthManagementException {
		try {
			getManageableRealm().assignRoleToUser(username, role);
		} catch (final ForwardedRuntimeException e) {
			exceptionRegistry.throwRuntimeException(e);
		}
	}

	@Override
	public void removeRoleFromUser(final String username, final String role)
			throws AuthManagementException {
		try {
			getManageableRealm().removeRoleFromUser(username, role);
		} catch (final ForwardedRuntimeException e) {
			exceptionRegistry.throwRuntimeException(e);
		}
	}

	@Override
	public void grantPermissionsToUser(final String username,
			final String[] permissions) throws AuthManagementException,
			PermissionException {
		try {
			getManageableRealm().grantPermissionsToUser(username, permissions);
		} catch (final ForwardedRuntimeException e) {
			exceptionRegistry.throwRuntimeException(e);
		}
	}

	@Override
	public void revokePermissionsFromUser(final String username,
			final String[] permissions) throws AuthManagementException,
			PermissionException {
		try {
			getManageableRealm().revokePermissionsFromUser(username,
					permissions);
		} catch (final ForwardedRuntimeException e) {
			exceptionRegistry.throwRuntimeException(e);
		}
	}

	@Override
	public void grantPermissionsToRole(final String role,
			final String[] permissions) throws AuthManagementException,
			PermissionException {
		try {
			getManageableRealm().grantPermissionsToRole(role, permissions);
		} catch (final ForwardedRuntimeException e) {
			exceptionRegistry.throwRuntimeException(e);
		}
	}

	@Override
	public void revokePermissionsFromRole(final String role,
			final String[] permissions) throws AuthManagementException,
			PermissionException {
		try {
			getManageableRealm().revokePermissionsFromRole(role, permissions);
		} catch (final ForwardedRuntimeException e) {
			exceptionRegistry.throwRuntimeException(e);
		}
	}

	/**
	 * Removes everything (i.e. roles and users) from the realm.
	 */
	protected void clear() {
		getManageableRealm().clear();
	}
}
