package net.meisen.dissertation.impl.auth.shiro;

import java.io.File;
import java.util.Arrays;
import java.util.Collection;
import java.util.Map;
import java.util.Set;

import net.meisen.dissertation.exceptions.AuthManagementException;
import net.meisen.dissertation.exceptions.PermissionException;
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

public class MapDbAuthorizingRealm extends AuthorizingRealm implements
		Destroyable {
	public final static String adminName = "admin";
	public final static String adminPassword = "fuckyou";

	private final static Logger LOG = LoggerFactory
			.getLogger(MapDbAuthorizingRealm.class);
	private final static String dbFile = "mapDbAuth.db";

	private final DB db;
	protected final Map<String, SimpleAccount> users;
	protected final Map<String, SimpleRole> roles;

	public MapDbAuthorizingRealm(final File location) {
		this(Files.getCanonicalPath(location));
	}

	public MapDbAuthorizingRealm(final File location, final String name) {
		this(Files.getCanonicalPath(location));
		setName(name);
	}

	public MapDbAuthorizingRealm(final String location, final String name) {
		this(location);
		setName(name);
	}

	public MapDbAuthorizingRealm(final String location) {

		// create the maker of the database and create it
		final DBMaker<?> maker = DBMaker.newFileDB(new File(location, dbFile));
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
	public void destroy() {
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

		if (this.users.containsKey(name)) {
			throw new ForwardedRuntimeException(AuthManagementException.class,
					1000, name);
		} else {

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
	 */
	public void deleteUser(final String name) {
		this.users.remove(name);
		db.commit();
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
		final SimpleAccount account = this.users.get(name);

		if (account == null) {
			throw new ForwardedRuntimeException(AuthManagementException.class,
					1001, name);
		} else {

			// set the new credentials
			account.setCredentials(password);

			// remove and add the modified account
			this.users.remove(name);
			this.users.put(name, account);
			db.commit();
		}
	}

	public void addRole(final String role, final String[] permissions)
			throws AuthManagementException, PermissionException {

		if (this.roles.containsKey(role)) {
			throw new ForwardedRuntimeException(AuthManagementException.class,
					1002, role);
		} else {
			final Set<Permission> perms = PermissionUtils.resolvePermissions(
					Arrays.asList(permissions), getPermissionResolver());

			add(new SimpleRole(role, perms));
		}
	}

	protected void add(final SimpleRole role) {
		roles.put(role.getName(), role);

		db.commit();
	}

	@Override
	protected AuthenticationInfo doGetAuthenticationInfo(
			final AuthenticationToken token) throws AuthenticationException {
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
		final String username = getAvailablePrincipal(principals).toString();
		return this.users.get(username);
	}

	/**
	 * Clears all the data of the realm.
	 */
	public void clear() {
		this.users.clear();
		this.roles.clear();
		db.commit();

		// add the administrator again
		checkAdministrator();
	}

	protected void checkAdministrator() {

		// check if we have values already, otherwise add an administrator-role
		if (this.roles.size() == 0) {
			addRole("administrator", new String[] { "*" });
		}

		// check if we have values already, otherwise add an administrator
		if (this.users.size() == 0) {
			addUser(adminName, adminPassword, new String[] { "administrator" },
					null);
		}
	}
}