package net.meisen.dissertation.impl.auth.shiro;

import java.io.File;
import java.util.Arrays;
import java.util.Collection;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import net.meisen.dissertation.exceptions.AuthManagementException;
import net.meisen.dissertation.model.auth.permissions.DefinedPermission;
import net.meisen.general.genmisc.exceptions.ForwardedRuntimeException;
import net.meisen.general.genmisc.types.Files;

import org.apache.shiro.authc.AuthenticationException;
import org.apache.shiro.authc.AuthenticationInfo;
import org.apache.shiro.authc.AuthenticationToken;
import org.apache.shiro.authc.ExpiredCredentialsException;
import org.apache.shiro.authc.LockedAccountException;
import org.apache.shiro.authc.SimpleAccount;
import org.apache.shiro.authc.UsernamePasswordToken;
import org.apache.shiro.authz.AuthorizationInfo;
import org.apache.shiro.authz.Permission;
import org.apache.shiro.authz.SimpleRole;
import org.apache.shiro.authz.permission.PermissionResolver;
import org.apache.shiro.authz.permission.RolePermissionResolver;
import org.apache.shiro.realm.AuthorizingRealm;
import org.apache.shiro.subject.PrincipalCollection;
import org.apache.shiro.util.Destroyable;
import org.apache.shiro.util.PermissionUtils;
import org.mapdb.DB;
import org.mapdb.DB.HTreeMapMaker;
import org.mapdb.DBMaker;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * A realm usable as shiro realm which uses mapDb to store the realms
 * information.
 * 
 * @author pmeisen
 * 
 */
public class MapDbAuthorizingRealm extends AuthorizingRealm implements
		Destroyable {

	/**
	 * The name of the administrator account, created if no other account is
	 * specified.
	 */
	protected final static String adminName = "admin";
	/**
	 * The password of the administrator account, which is created if no other
	 * account exists.
	 */
	protected final static String adminPassword = "password";

	private final static Logger LOG = LoggerFactory
			.getLogger(MapDbAuthorizingRealm.class);
	private final static String dbFile = "mapDbAuth.db";

	private final File location;

	private DB db;
	private Map<String, SimpleAccount> users;
	private Map<String, SimpleRole> roles;

	/**
	 * Constructor specifying the {@code location}, where the files of the users
	 * should be stored.
	 * 
	 * @param location
	 *            the location to store the files at
	 */
	public MapDbAuthorizingRealm(final File location) {
		this(Files.getCanonicalPath(location));
	}

	/**
	 * Constructor specifying the {@code location}, where the files of the users
	 * should be stored, and the name of the realm.
	 * 
	 * @param location
	 *            the location to store the files at
	 * @param name
	 *            the name of the realm
	 */
	public MapDbAuthorizingRealm(final File location, final String name) {
		this(Files.getCanonicalPath(location));
		setName(name);
	}

	/**
	 * Constructor specifying the {@code location}, where the files of the users
	 * should be stored, and the name of the realm.
	 * 
	 * @param location
	 *            the location to store the files at
	 * @param name
	 *            the name of the realm
	 */
	public MapDbAuthorizingRealm(final String location, final String name) {
		this(location);
		setName(name);
	}

	/**
	 * Constructor specifying the {@code location}, where the files of the users
	 * should be stored.
	 * 
	 * @param location
	 *            the location to store the files at
	 */
	public MapDbAuthorizingRealm(final String location) {
		this.location = new File(location, dbFile);

		// make the permission resolver case sensitive
		setPermissionResolver(new PermissionResolver() {

			@Override
			public Permission resolvePermission(final String permissionString) {
				return new ExtendedWildcardPermission(permissionString);
			}
		});
	}

	/**
	 * Validates the specified {@code role} to be used as role.
	 * 
	 * @param role
	 *            the role to be validated
	 * 
	 * @return {@code true} if the role is valid, otherwise {@code false}
	 */
	public boolean validateRole(final String role) {
		if (role == null || role.trim().isEmpty()) {
			return false;
		} else {
			return true;
		}
	}

	/**
	 * Validates the specified {@code name} to be used as user-name.
	 * 
	 * @param name
	 *            the user-name to be validated
	 * 
	 * @return {@code true} if the user-name is valid, otherwise {@code false}
	 */
	public boolean validateUser(final String name) {
		if (name == null || name.trim().isEmpty()) {
			return false;
		} else {
			return true;
		}
	}

	/**
	 * Helper method which is used to cast a {@code Collection} to a {@code Set}
	 * . If the {@code Collection} isn't an instance of a {@code Set}.
	 * 
	 * @param coll
	 *            the {@code Collection} to be casted
	 * 
	 * @return the casted collection or a set
	 */
	protected <T> Set<T> cast(final Collection<T> coll) {
		if (coll == null) {
			return null;
		} else if (coll instanceof Set) {
			return (Set<T>) coll;
		} else {
			return new LinkedHashSet<T>(coll);
		}
	}

	@Override
	public void onInit() {
		if (LOG.isTraceEnabled()) {
			LOG.trace("Initializing the '"
					+ MapDbAuthorizingRealm.class.getSimpleName() + "'.");
		}

		super.onInit();

		// create the maker of the database and create it
		final DBMaker<?> maker = DBMaker.newFileDB(location);
		maker.cacheHardRefEnable();
		maker.cacheLRUEnable();
		maker.cacheSize(1000);
		maker.closeOnJvmShutdown();
		db = maker.make();

		// create the maker for the specific maps of users and roles
		final HTreeMapMaker usersMapMaker = db.createHashMap("users");
		final HTreeMapMaker rolesMapMaker = db.createHashMap("roles");
		this.users = usersMapMaker.makeOrGet();
		this.roles = rolesMapMaker.makeOrGet();

		// disable any caching
		setCachingEnabled(false);
		setCacheManager(null);

		// add a role resolver
		setRolePermissionResolver(new RolePermissionResolver() {

			@Override
			public Collection<Permission> resolvePermissionsInRole(
					final String role) {

				final SimpleRole r = roles.get(role);
				if (r == null) {
					return null;
				} else {
					return r.getPermissions();
				}
			}
		});

		checkAdministrator();
	}

	@Override
	public void destroy() throws Exception {
		this.release();
	}

	/**
	 * Releases all the resources used by the realm.
	 */
	public void release() {
		if (LOG.isTraceEnabled()) {
			LOG.trace("Destroying the '"
					+ MapDbAuthorizingRealm.class.getSimpleName() + "'.");
		}

		// close the database
		db.close();
	}

	/**
	 * Adds a user to the realm using the specified {@code name} and
	 * {@code password}. It is possible to assign {@code roles} to the user, as
	 * well as {@code permissions}.
	 * 
	 * @param name
	 *            the name of the user
	 * @param password
	 *            the password
	 * @param roles
	 *            the roles, can be {@code null}
	 * @param permissions
	 *            the permissions, can be {@code null}
	 * 
	 * @throws ForwardedRuntimeException
	 *             if an other already exists with the same {@code name}, the
	 *             exception wraps a {@code AuthManagementException}
	 */
	public void addUser(final String name, final String password,
			final String[] roles, final String[] permissions)
			throws ForwardedRuntimeException {

		if (isClosed()) {
			throw new ForwardedRuntimeException(AuthManagementException.class,
					1003);
		} else if (!validateUser(name)) {
			throw new ForwardedRuntimeException(AuthManagementException.class,
					1007, name);
		} else if (this.users.containsKey(name)) {
			throw new ForwardedRuntimeException(AuthManagementException.class,
					1000, name);
		} else {
			if (LOG.isTraceEnabled()) {
				LOG.trace("Adding user '"
						+ name
						+ "' with roles '"
						+ (roles == null ? null : Arrays.asList(roles))
						+ "' and permissions '"
						+ (permissions == null ? null : Arrays
								.asList(permissions)) + "'.");
			}

			// create the account
			final SimpleAccount account = new SimpleAccount(name, password,
					getName());

			// set the roles
			if (roles != null && roles.length > 0) {
				account.addRole(Arrays.asList(roles));
			}

			// set the permissions for the account
			if (permissions != null && permissions.length > 0) {
				final Set<Permission> perms = PermissionUtils
						.resolvePermissions(Arrays.asList(permissions),
								getPermissionResolver());
				account.setObjectPermissions(perms);
			}

			// add the user to the map
			this.users.put(name, account);
			db.commit();
		}
	}

	/**
	 * Deletes the user from the realm.
	 * 
	 * @param name
	 *            the user to be removed
	 * 
	 * @throws AuthManagementException
	 *             if the realm is closed
	 */
	public void deleteUser(final String name) throws AuthManagementException {
		if (isClosed()) {
			throw new ForwardedRuntimeException(AuthManagementException.class,
					1003);
		} else {
			if (LOG.isTraceEnabled()) {
				LOG.trace("Deleting user '" + name + "'.");
			}

			this.users.remove(name);
			db.commit();
		}
	}

	/**
	 * Changes the password of a user.
	 * 
	 * @param name
	 *            the name of the user to be modified
	 * @param password
	 *            the new password of the user
	 * 
	 * @throws ForwardedRuntimeException
	 *             if a user with {@code name} does not exist, the exception
	 *             wraps a {@code AuthManagementException}
	 */
	public void modifyPassword(final String name, final String password)
			throws ForwardedRuntimeException {
		if (isClosed()) {
			throw new ForwardedRuntimeException(AuthManagementException.class,
					1003);
		}

		final SimpleAccount account = this.users.get(name);

		if (account == null) {
			throw new ForwardedRuntimeException(AuthManagementException.class,
					1001, name);
		} else {
			if (LOG.isTraceEnabled()) {
				LOG.trace("Modifying password of user '" + name + "'.");
			}

			// set the new credentials
			final SimpleAccount clone = clone(account);
			clone.setCredentials(password);

			this.users.put(name, clone);
			db.commit();
		}
	}

	/**
	 * Adds a role to the realm.
	 * 
	 * @param role
	 *            the name of the role to be added
	 * 
	 * @param permissions
	 *            the permissions assign to the role
	 * @throws AuthManagementException
	 *             if another role with the same name already exists
	 */
	public void addRole(final String role, final String[] permissions)
			throws AuthManagementException {

		if (isClosed()) {
			throw new ForwardedRuntimeException(AuthManagementException.class,
					1003);
		} else if (!validateRole(role)) {
			throw new ForwardedRuntimeException(AuthManagementException.class,
					1006, role);
		} else if (this.roles.containsKey(role)) {
			throw new ForwardedRuntimeException(AuthManagementException.class,
					1002, role);
		} else {
			if (LOG.isTraceEnabled()) {
				LOG.trace("Adding role '"
						+ role
						+ "' with permissions '"
						+ (permissions == null ? null : Arrays
								.asList(permissions)) + ".");
			}

			final Set<Permission> perms = PermissionUtils.resolvePermissions(
					Arrays.asList(permissions), getPermissionResolver());

			final SimpleRole r = new SimpleRole(role, perms);
			roles.put(role, r);
			db.commit();
		}
	}

	/**
	 * Deletes the role from the realm.
	 * 
	 * @param role
	 *            the role to be removed
	 * 
	 * @throws AuthManagementException
	 *             if the realm is closed
	 */
	public void deleteRole(final String role) throws AuthManagementException {
		if (isClosed()) {
			throw new ForwardedRuntimeException(AuthManagementException.class,
					1003);
		} else {
			if (LOG.isTraceEnabled()) {
				LOG.trace("Removing role '" + role + "'.");
			}

			this.roles.remove(role);
			db.commit();
		}
	}

	/**
	 * Assigns the specified {@code role} to the specified {@code username}.
	 * 
	 * @param username
	 *            the name of the user to assign the role to
	 * @param role
	 *            the role to be assigned
	 * 
	 * @throws AuthManagementException
	 *             if the realm is closed or if the user does not exist
	 */
	public void assignRoleToUser(final String username, final String role)
			throws AuthManagementException {
		if (isClosed()) {
			throw new ForwardedRuntimeException(AuthManagementException.class,
					1003);
		} else if (!validateRole(role)) {
			throw new ForwardedRuntimeException(AuthManagementException.class,
					1006, role);
		}
		final SimpleAccount account = this.users.get(username);

		if (account == null) {
			throw new ForwardedRuntimeException(AuthManagementException.class,
					1004, role, username);
		} else {
			if (LOG.isTraceEnabled()) {
				LOG.trace("Assigning role '" + role + "' to user '" + username
						+ "'.");
			}
			final SimpleAccount clone = clone(account);
			clone.addRole(role);

			this.users.put(username, clone);
			db.commit();
		}
	}

	/**
	 * Removes the specified {@code role} from the specified {@code username}.
	 * 
	 * @param username
	 *            the name of the user to remove the role from
	 * @param role
	 *            the role to be removed
	 * 
	 * @throws AuthManagementException
	 *             if the realm is closed or if the user does not exist
	 */
	public void removeRoleFromUser(final String username, final String role)
			throws AuthManagementException {
		if (isClosed()) {
			throw new ForwardedRuntimeException(AuthManagementException.class,
					1003);
		}
		final SimpleAccount account = this.users.get(username);
		if (account == null) {
			throw new ForwardedRuntimeException(AuthManagementException.class,
					1005, role, username);
		} else {
			if (LOG.isTraceEnabled()) {
				LOG.trace("Removing role '" + role + "' from user '" + username
						+ "'.");
			}
			final SimpleAccount clone = clone(account);
			final Collection<String> roles = clone.getRoles();
			if (roles != null && roles.size() > 0) {

				// we need a set so get one
				final Set<String> setOfRoles = cast(roles);
				setOfRoles.remove(role);

				// set null if there aren't any
				if (setOfRoles.size() == 0) {
					clone.setRoles(null);
				} else {
					clone.setRoles(setOfRoles);
				}

				this.users.put(username, clone);
				db.commit();
			}
		}
	}

	/**
	 * Grants the specified {@code permissions} to the specified
	 * {@code username}.
	 * 
	 * @param username
	 *            the name of the user to grant the permissions to
	 * @param permissions
	 *            the permissions to be granted (cannot be {@code null} or
	 *            empty)
	 * 
	 * @throws AuthManagementException
	 *             if the permissions are empty, if the {@code username} does
	 *             not exist or if the realm is closed
	 */
	public void grantPermissionsToUser(final String username,
			final String[] permissions) throws AuthManagementException {

		if (isClosed()) {
			throw new ForwardedRuntimeException(AuthManagementException.class,
					1003);
		} else if (permissions == null || permissions.length == 0) {
			throw new ForwardedRuntimeException(AuthManagementException.class,
					1008, username);
		}
		final SimpleAccount account = this.users.get(username);

		if (account == null) {
			throw new ForwardedRuntimeException(AuthManagementException.class,
					1009, Arrays.asList(permissions), username);
		} else {
			final List<String> perms = Arrays.asList(permissions);

			if (LOG.isTraceEnabled()) {
				LOG.trace("Granting '" + perms + "' to user '" + username
						+ "'.");
			}
			final SimpleAccount clone = clone(account);
			clone.addObjectPermissions(PermissionUtils.resolvePermissions(
					perms, getPermissionResolver()));

			this.users.put(username, clone);
			db.commit();
		}
	}

	/**
	 * Revokes the specified {@code permissions} from the specified
	 * {@code username}.
	 * 
	 * @param username
	 *            the name of the user to revoke the permissions from
	 * @param permissions
	 *            the permissions to be revoked
	 * 
	 * @throws AuthManagementException
	 *             if the {@code username} does not exist or if the realm is
	 *             closed
	 */
	public void revokePermissionsFromUser(final String username,
			final String[] permissions) throws AuthManagementException {

		if (isClosed()) {
			throw new ForwardedRuntimeException(AuthManagementException.class,
					1003);
		} else if (permissions == null || permissions.length == 0) {
			return;
		}
		final SimpleAccount account = this.users.get(username);

		if (account == null) {
			throw new ForwardedRuntimeException(AuthManagementException.class,
					1010, Arrays.asList(permissions), username);
		} else {
			if (LOG.isTraceEnabled()) {
				LOG.trace("Revoking '" + Arrays.asList(permissions)
						+ "' from user '" + username + "'.");
			}
			final SimpleAccount clone = clone(account);

			// get the permissions of the account
			final Set<Permission> objPerms = cast(clone.getObjectPermissions());
			final Set<String> strPerms = cast(clone.getStringPermissions());

			// get the defined permissions to be revoked as ObjectPermissions
			final Set<Permission> objPermissions = PermissionUtils
					.resolvePermissions(Arrays.asList(permissions),
							getPermissionResolver());

			// check the ObjectPermissions and remove the permissions there
			if (objPerms != null) {
				for (final Permission perm : objPermissions) {
					objPerms.remove(perm);
				}
				clone.setObjectPermissions(objPerms);
			}

			// check the StringPermissions and remove the permissions there
			if (strPerms != null) {
				for (final String perm : permissions) {
					strPerms.remove(perm);
				}
				clone.setStringPermissions(strPerms);
			}

			this.users.put(username, clone);
			db.commit();
		}
	}

	/**
	 * Grants the specified {@code permissions} to the specified {@code role}.
	 * 
	 * @param role
	 *            the role to apply the {@code permissions} to
	 * @param permissions
	 *            the permissions to be applied
	 * 
	 * @throws AuthManagementException
	 *             if the realm is closed, if the specified {@code role} does
	 *             not exist or if the defined {@code permissions} are
	 *             {@code null} or empty
	 */
	public void grantPermissionsToRole(final String role,
			final String[] permissions) throws AuthManagementException {
		if (isClosed()) {
			throw new ForwardedRuntimeException(AuthManagementException.class,
					1003);
		} else if (permissions == null || permissions.length == 0) {
			throw new ForwardedRuntimeException(AuthManagementException.class,
					1013, role);
		}

		final SimpleRole simpleRole = this.roles.get(role);
		if (simpleRole == null) {
			throw new ForwardedRuntimeException(AuthManagementException.class,
					1011, Arrays.asList(permissions), role);
		} else {
			final SimpleRole clone = clone(simpleRole);
			final Set<Permission> perms = PermissionUtils.resolvePermissions(
					Arrays.asList(permissions), getPermissionResolver());
			clone.addAll(perms);

			this.roles.put(role, clone);
			db.commit();
		}
	}

	/**
	 * Revokes the specified {@code permissions} from the specified {@code role}
	 * .
	 * 
	 * @param role
	 *            the role to revoke the {@code permissions} from
	 * @param permissions
	 *            the permissions to be revoked
	 * 
	 * @throws AuthManagementException
	 *             if the realm is closed or if the specified {@code role} does
	 *             not exist
	 */
	public void revokePermissionsFromRole(final String role,
			final String[] permissions) throws AuthManagementException {
		if (isClosed()) {
			throw new ForwardedRuntimeException(AuthManagementException.class,
					1003);
		} else if (permissions == null || permissions.length == 0) {
			return;
		}

		final SimpleRole simpleRole = this.roles.get(role);
		if (simpleRole == null) {
			throw new ForwardedRuntimeException(AuthManagementException.class,
					1012, Arrays.asList(permissions), role);
		} else {
			final SimpleRole clone = clone(simpleRole);
			final Set<Permission> perms = PermissionUtils.resolvePermissions(
					Arrays.asList(permissions), getPermissionResolver());
			final Set<Permission> rolePermissions = clone.getPermissions();
			rolePermissions.removeAll(perms);
			clone.setPermissions(rolePermissions);

			this.roles.put(role, clone);
			db.commit();
		}
	}

	/**
	 * Checks if the specified {@code role} has the specified {@code permission}
	 * .
	 * 
	 * @param role
	 *            the role to be checked for permission
	 * @param permission
	 *            the permission to be checked
	 * 
	 * @return {@code true} if the role has the permission, otherwise
	 *         {@code false}
	 */
	public boolean isPermitted(final String role, final String permission) {
		final SimpleRole simpleRole = this.roles.get(role);
		if (simpleRole == null) {
			return false;
		}

		final Permission perm = getPermissionResolver().resolvePermission(
				permission);
		return simpleRole.isPermitted(perm);
	}

	/**
	 * Clones a {@code SimpleAccount} instance. The MapDb documentation suggests
	 * that a persisted instance should be immutable.
	 * 
	 * @param account
	 *            the account to be cloned
	 * 
	 * @return the cloned instance
	 */
	protected SimpleAccount clone(final SimpleAccount account) {
		if (account == null) {
			return null;
		}

		final Set<String> roles;
		if (account.getRoles() == null) {
			roles = null;
		} else {
			roles = new LinkedHashSet<String>();
			roles.addAll(account.getRoles());
		}
		final Set<Permission> permissions;
		if (account.getObjectPermissions() == null) {
			permissions = null;
		} else {
			permissions = new LinkedHashSet<Permission>();
			permissions.addAll(account.getObjectPermissions());
		}

		return new SimpleAccount(account.getPrincipals(),
				account.getCredentials(), roles, permissions);
	}

	/**
	 * Clones the specified {@code role}.
	 * 
	 * @param role
	 *            the role to be cloned
	 * 
	 * @return the clone
	 */
	protected SimpleRole clone(final SimpleRole role) {
		if (role == null) {
			return null;
		}

		final Set<Permission> permissions;
		if (role.getPermissions() == null) {
			permissions = null;
		} else {
			permissions = new LinkedHashSet<Permission>();
			permissions.addAll(role.getPermissions());
		}

		return new SimpleRole(role.getName(), permissions);
	}

	@Override
	protected AuthenticationInfo doGetAuthenticationInfo(
			final AuthenticationToken token) throws AuthenticationException {
		if (isClosed()) {
			throw new ForwardedRuntimeException(AuthManagementException.class,
					1003);
		}

		final UsernamePasswordToken upToken = (UsernamePasswordToken) token;
		final SimpleAccount account = this.users.get(upToken.getUsername());

		if (account != null) {

			if (account.isLocked()) {
				throw new LockedAccountException("Account [" + account
						+ "] is locked.");
			}
			if (account.isCredentialsExpired()) {
				throw new ExpiredCredentialsException(
						"The credentials for account [" + account
								+ "] are expired");
			}

		}

		return account;
	}

	@Override
	protected AuthorizationInfo doGetAuthorizationInfo(
			final PrincipalCollection principals) {
		if (isClosed()) {
			throw new ForwardedRuntimeException(AuthManagementException.class,
					1003);
		}

		final String username = getAvailablePrincipal(principals).toString();
		return this.users.get(username);
	}

	/**
	 * Clears all the data of the realm.
	 */
	public void clear() {
		this.users.clear();
		this.roles.clear();

		/*
		 * Check if the administrator was added, if so a commit was triggered.
		 * Otherwise it still has to be done.
		 */
		if (!checkAdministrator()) {
			db.commit();
		}
	}

	/**
	 * Checks if the realm is closed, i.e. if no modifications or retrievals can
	 * be processed anymore.
	 * 
	 * @return {@code true} if it's closed, otherwise {@code true}
	 */
	public boolean isClosed() {
		return db == null || db.isClosed();
	}

	/**
	 * Checks if there is at least one user. If not an administrator (i.e. all
	 * permissions are granted) using {@link #adminName} and
	 * {@link #adminPassword} is created and committed.
	 * 
	 * @return {@code true} if the admin was added, otherwise {@code false}
	 * 
	 * @throws AuthManagementException
	 *             if the underlying database is closed
	 */
	protected boolean checkAdministrator() throws AuthManagementException {
		if (isClosed()) {
			throw new ForwardedRuntimeException(AuthManagementException.class,
					1003);
		} else if (this.users.size() == 0) {
			addUser(adminName, adminPassword, null, new String[] { "*" });
			return true;
		}

		return false;
	}

	/**
	 * Get all the users known by the realm.
	 * 
	 * @return all the users known by the realm
	 */
	public Set<String> getUsers() {
		final Set<String> users = new LinkedHashSet<String>();
		users.addAll(this.users.keySet());

		return users;
	}

	/**
	 * Get all the roles known by the realm.
	 * 
	 * @return all the roles known by the realm
	 */
	public Set<String> getRoles() {
		final Set<String> roles = new LinkedHashSet<String>();
		roles.addAll(this.roles.keySet());

		return roles;
	}

	/**
	 * Gets the roles assigned to the specified {@code username}.
	 * 
	 * @param username
	 *            the name of the user to retrieve the roles for
	 * 
	 * @return the roles of the user
	 */
	public Set<String> getUserRoles(final String username) {
		final SimpleAccount account = this.users.get(username);
		final Set<String> roles = new LinkedHashSet<String>();

		if (account != null) {
			final Collection<String> accountRoles = account.getRoles();
			if (accountRoles != null) {
				roles.addAll(accountRoles);
			}
		}

		return roles;
	}

	/**
	 * Gets the permissions assigned to the specified {@code username}.
	 * 
	 * @param username
	 *            the name of the user to retrieve the permissions for
	 * @param separator
	 *            the separator used to separate the different parts of the
	 *            permission
	 * 
	 * @return the permissions of the user
	 */
	public Set<String> getUserPermissions(final String username,
			final String separator) {
		return getUserPermissions(username, separator, true, true);
	}

	/**
	 * Gets the permissions assigned to the specified {@code username}.
	 * 
	 * @param username
	 *            the name of the user to retrieve the permissions for
	 * @param separator
	 *            the separator used to separate the different parts of the
	 *            permission
	 * @param directlyAssignedPermissions
	 *            {@code true} if the directly assigned permissions should be
	 *            included, otherwise {@code false}
	 * @param rolePermissions
	 *            {@code true} if the role permissions should be included,
	 *            otherwise {@code false}
	 * 
	 * @return the permissions of the user
	 */
	public Set<String> getUserPermissions(final String username,
			final String separator, final boolean directlyAssignedPermissions,
			final boolean rolePermissions) {
		final SimpleAccount account = this.users.get(username);
		final Set<String> permissions = new LinkedHashSet<String>();

		if (account != null) {
			if (directlyAssignedPermissions) {
				final Collection<Permission> collPerm = account
						.getObjectPermissions();
				if (collPerm != null) {
					for (final Permission perm : collPerm) {
						permissions.addAll(resolvePermission(perm, separator));
					}
				}

				final Collection<String> collStr = account
						.getStringPermissions();
				if (collStr != null) {
					for (final String perm : collStr) {
						permissions.add(perm.toString());
					}
				}
			}

			if (rolePermissions) {
				final Collection<String> collStr = account.getRoles();
				if (collStr != null) {
					for (final Object role : collStr) {
						final SimpleRole r = this.roles.get(role);
						final Collection<Permission> collPerm = r == null ? null
								: r.getPermissions();
						if (collPerm != null) {
							for (final Permission perm : collPerm) {
								permissions.addAll(resolvePermission(perm,
										separator));
							}
						}
					}
				}
			}
		}

		return permissions;
	}

	/**
	 * Get the permissions associated to the specified role.
	 * 
	 * @param role
	 *            the name of the role
	 * @param separator
	 *            the separator to be used to separate the different permissions
	 * 
	 * @return the set of permissions
	 */
	public Set<String> getRolePermissions(final String role,
			final String separator) {
		final Collection<Permission> perms = getRolePermissionResolver()
				.resolvePermissionsInRole(role);

		final Set<String> permissions = new LinkedHashSet<String>();
		for (final Permission perm : perms) {
			permissions.addAll(resolvePermission(perm, separator));
		}

		return permissions;
	}

	/**
	 * Resolve the specified {@code perm} to a set of {@code Permissions}.
	 * 
	 * @param perm
	 *            the {@code ExtendedWildcardPermission} to resolve the
	 *            permissions from
	 * @param separator
	 *            the separator used to separate the different parts of the
	 *            permission
	 * 
	 * @return the resolved permissions
	 * 
	 * @see ExtendedWildcardPermission
	 */
	protected Set<String> resolvePermission(final Permission perm,
			final String separator) {
		final Set<String> permissions = new LinkedHashSet<String>();

		if (perm == null) {
			// do nothing
		} else {
			for (final net.meisen.dissertation.model.auth.permissions.Permission sysPerm : net.meisen.dissertation.model.auth.permissions.Permission
					.values()) {

				final String strPerm = (sysPerm.isGlobal() ? sysPerm.create()
						: sysPerm.create("*")).toString(separator);
				final Set<Permission> resPerms = PermissionUtils
						.resolveDelimitedPermissions(strPerm,
								getPermissionResolver());

				final Permission resPerm;
				if (resPerms == null || resPerms.size() != 1) {
					continue;
				} else {
					resPerm = resPerms.iterator().next();
				}

				if (perm.implies(resPerm) || resPerm.implies(perm)) {

					// check if the perm contains a model
					if (perm instanceof ExtendedWildcardPermission) {
						final ExtendedWildcardPermission wPerm = (ExtendedWildcardPermission) perm;
						final DefinedPermission defPerm = wPerm.get();

						if (defPerm == null) {
							permissions.add(strPerm);
						} else {
							permissions.add(defPerm.toString(separator));
						}
					}
				}
			}
		}

		return permissions;
	}
}