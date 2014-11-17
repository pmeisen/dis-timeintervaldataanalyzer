package net.meisen.dissertation.impl.auth.shiro;

import java.util.LinkedHashSet;
import java.util.Set;

import net.meisen.dissertation.config.xslt.DefaultValues;
import net.meisen.dissertation.exceptions.AuthException;
import net.meisen.dissertation.exceptions.AuthManagementException;
import net.meisen.dissertation.exceptions.PermissionException;
import net.meisen.dissertation.model.auth.IAuthManager;
import net.meisen.dissertation.model.auth.IAuthManagerConfig;
import net.meisen.dissertation.model.auth.permissions.DefinedPermission;
import net.meisen.dissertation.model.auth.permissions.Permission;
import net.meisen.dissertation.model.handler.TidaModelHandler;
import net.meisen.dissertation.server.sessions.Session;
import net.meisen.general.genmisc.exceptions.ForwardedRuntimeException;
import net.meisen.general.genmisc.exceptions.registry.IExceptionRegistry;

import org.apache.shiro.authc.AuthenticationException;
import org.apache.shiro.authc.IncorrectCredentialsException;
import org.apache.shiro.authc.LockedAccountException;
import org.apache.shiro.authc.UnknownAccountException;
import org.apache.shiro.authc.UsernamePasswordToken;
import org.apache.shiro.mgt.DefaultSecurityManager;
import org.apache.shiro.mgt.DefaultSessionStorageEvaluator;
import org.apache.shiro.mgt.DefaultSubjectDAO;
import org.apache.shiro.mgt.SessionStorageEvaluator;
import org.apache.shiro.mgt.SubjectDAO;
import org.apache.shiro.realm.Realm;
import org.apache.shiro.session.mgt.AbstractValidatingSessionManager;
import org.apache.shiro.session.mgt.SessionManager;
import org.apache.shiro.subject.Subject;
import org.apache.shiro.subject.Subject.Builder;
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

	/**
	 * The separator used to separate the permission levels internally.
	 */
	protected final static String permissionSeparator = ":";

	@Autowired
	@Qualifier(DefaultValues.MODELHANDLER_ID)
	private TidaModelHandler handler;

	@Autowired
	@Qualifier(DefaultValues.EXCEPTIONREGISTRY_ID)
	private IExceptionRegistry exceptionRegistry;

	private DefaultSecurityManager manager;

	private Builder builder;

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
		final SessionManager sessionManager = this.manager.getSessionManager();
		if (sessionManager instanceof AbstractValidatingSessionManager) {
			((AbstractValidatingSessionManager) sessionManager)
					.setSessionValidationSchedulerEnabled(false);
		}
		final SubjectDAO subjectDao = this.manager.getSubjectDAO();
		if (subjectDao instanceof DefaultSubjectDAO) {
			final SessionStorageEvaluator sessionStorageEvaluator = ((DefaultSubjectDAO) this.manager
					.getSubjectDAO()).getSessionStorageEvaluator();
			if (sessionStorageEvaluator instanceof DefaultSessionStorageEvaluator) {
				((DefaultSessionStorageEvaluator) sessionStorageEvaluator)
						.setSessionStorageEnabled(false);
			}
		}
		this.builder = new Subject.Builder(manager);
		this.builder.sessionCreationEnabled(false);
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

	@Override
	public String getCurrentUsername() {
		final Subject currentUser = getSubject();
		final Object principal = currentUser.getPrincipal();

		if (principal == null) {
			return null;
		} else {
			return principal.toString();
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
			subject = this.builder.buildSubject();
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
	public void bind(final Session session) throws AuthException {
		if (session == null) {
			exceptionRegistry.throwRuntimeException(AuthException.class, 1000);
		} else {

			// check if there is a subject known
			final Subject subject = session.get("subject");

			// if not create one and bind it, ...
			if (subject == null) {
				session.bind("subject", getSubject());
			}
			// otherwise bind it now
			else {
				ThreadContext.bind(manager);
				ThreadContext.bind(subject);
			}
		}
	}

	@Override
	public void unbind() {
		ThreadContext.unbindSubject();
		ThreadContext.unbindSecurityManager();
		ThreadContext.remove();
	}

	@Override
	public void logout() {
		final Subject currentUser = getSubject();
		currentUser.logout();

		unbind();
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
			final DefinedPermission[] permissions)
			throws AuthManagementException, PermissionException {
		try {
			final String[] perms = Permission.transform(permissions,
					permissionSeparator);
			getManageableRealm().grantPermissionsToUser(username, perms);
		} catch (final ForwardedRuntimeException e) {
			exceptionRegistry.throwRuntimeException(e);
		}
	}

	@Override
	public void revokePermissionsFromUser(final String username,
			final DefinedPermission[] permissions)
			throws AuthManagementException, PermissionException {
		try {
			final String[] perms = Permission.transform(permissions,
					permissionSeparator);
			getManageableRealm().revokePermissionsFromUser(username, perms);
		} catch (final ForwardedRuntimeException e) {
			exceptionRegistry.throwRuntimeException(e);
		}
	}

	@Override
	public void grantPermissionsToRole(final String role,
			final DefinedPermission[] permissions)
			throws AuthManagementException, PermissionException {
		try {
			final String[] perms = Permission.transform(permissions,
					permissionSeparator);
			getManageableRealm().grantPermissionsToRole(role, perms);
		} catch (final ForwardedRuntimeException e) {
			exceptionRegistry.throwRuntimeException(e);
		}
	}

	@Override
	public void revokePermissionsFromRole(final String role,
			final DefinedPermission[] permissions)
			throws AuthManagementException, PermissionException {
		try {
			final String[] perms = Permission.transform(permissions,
					permissionSeparator);
			getManageableRealm().revokePermissionsFromRole(role, perms);
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

	@Override
	public Set<String> getUsers() {
		return getManageableRealm().getUsers();
	}

	@Override
	public Set<String> getRoles() {
		return getManageableRealm().getRoles();
	}

	@Override
	public Set<String> getUserRoles(final String username) {
		return getManageableRealm().getUserRoles(username);
	}

	@Override
	public Set<DefinedPermission> getUserPermissions(final String username) {
		final Set<String> permissions = getManageableRealm()
				.getUserPermissions(username, permissionSeparator);
		final Set<DefinedPermission> perms = new LinkedHashSet<DefinedPermission>();
		for (final String permission : permissions) {
			perms.add(DefinedPermission.fromString(permission,
					permissionSeparator));
		}

		return perms;
	}

	@Override
	public Set<DefinedPermission> getAssignedUserPermissions(
			final String username) {
		final Set<String> permissions = getManageableRealm()
				.getUserPermissions(username, permissionSeparator, true, false);
		final Set<DefinedPermission> perms = new LinkedHashSet<DefinedPermission>();
		for (final String permission : permissions) {
			perms.add(DefinedPermission.fromString(permission,
					permissionSeparator));
		}

		return perms;
	}

	@Override
	public Set<DefinedPermission> getRolePermissions(final String role) {
		final Set<String> permissions = getManageableRealm()
				.getRolePermissions(role, permissionSeparator);
		final Set<DefinedPermission> perms = new LinkedHashSet<DefinedPermission>();
		for (final String permission : permissions) {
			perms.add(DefinedPermission.fromString(permission,
					permissionSeparator));
		}

		return perms;
	}
}
