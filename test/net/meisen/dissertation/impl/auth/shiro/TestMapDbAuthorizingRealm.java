package net.meisen.dissertation.impl.auth.shiro;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;

import java.io.File;
import java.util.Collection;
import java.util.Set;
import java.util.UUID;

import net.meisen.dissertation.exceptions.AuthManagementException;
import net.meisen.dissertation.help.ExceptionBasedTest;
import net.meisen.dissertation.model.auth.permissions.DefinedPermission;
import net.meisen.general.genmisc.exceptions.ForwardedRuntimeException;
import net.meisen.general.genmisc.types.Files;

import org.apache.shiro.authc.AuthenticationInfo;
import org.apache.shiro.authc.SimpleAccount;
import org.apache.shiro.authc.UsernamePasswordToken;
import org.apache.shiro.authz.AuthorizationInfo;
import org.apache.shiro.authz.Permission;
import org.apache.shiro.subject.SimplePrincipalCollection;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;

/**
 * Tests the implementation of a {@code MapDbAuthorizingRealm}.
 * 
 * @author pmeisen
 * 
 */
public class TestMapDbAuthorizingRealm extends ExceptionBasedTest {
	private File realmDbDir;

	/**
	 * Creates a test-directory for the realm's database.
	 */
	@Before
	public void createTestDir() {

		// delete old stuff
		Files.deleteOnExitDir(new File(System.getProperty("java.io.tmpdir")),
				"testMapDbAuthorizingRealm-.*");

		// create a new directory
		realmDbDir = new File(System.getProperty("java.io.tmpdir"),
				"testMapDbAuthorizingRealm-" + UUID.randomUUID().toString());
		assertTrue(realmDbDir.mkdirs());
	}

	/**
	 * Test simple initialization and destruction of the realm.
	 */
	@Test
	public void testInitAndDestroy() {
		final MapDbAuthorizingRealm realm = new MapDbAuthorizingRealm(
				realmDbDir);

		// initialize the realm
		realm.init();

		// make sure the realm is destroyed
		realm.release();
	}

	/**
	 * Generally tests the settings of the realm.
	 */
	@Test
	public void testSettings() {
		final MapDbAuthorizingRealm realm = new MapDbAuthorizingRealm(
				realmDbDir);
		realm.init();

		assertFalse(realm.isCachingEnabled());
		assertNull(realm.getAuthenticationCache());
	}

	/**
	 * Tests the implementation of
	 * {@link MapDbAuthorizingRealm#addUser(String, String, String[], String[])}
	 * .
	 */
	@Test
	public void testAddUser() {
		Set<String> roles;

		final MapDbAuthorizingRealm realm = new MapDbAuthorizingRealm(
				realmDbDir);
		realm.init();

		// check if a user with name philipp is available
		final UsernamePasswordToken phTo = new UsernamePasswordToken("philipp",
				"password");
		final SimplePrincipalCollection phPr = new SimplePrincipalCollection(
				"philipp", realm.getName());
		assertNull(realm.doGetAuthenticationInfo(phTo));
		assertNull(realm.doGetAuthorizationInfo(phPr));

		// check a user without any permissions and roles
		realm.addUser("philipp", "password", null, null);
		assertNotNull(realm.doGetAuthenticationInfo(phTo));

		final AuthorizationInfo phInfo = realm.doGetAuthorizationInfo(phPr);
		assertNotNull(phInfo);
		assertNull(phInfo.getObjectPermissions());
		assertNull(phInfo.getRoles());

		// check a user with permissions and without roles
		final SimplePrincipalCollection toPr = new SimplePrincipalCollection(
				"tobias", realm.getName());
		realm.addUser("tobias", "password", null,
				new String[] { "dosomething" });

		final AuthorizationInfo toInfo = realm.doGetAuthorizationInfo(toPr);
		assertNotNull(toInfo);
		assertEquals(1, toInfo.getObjectPermissions().size());
		assertNull(toInfo.getRoles());

		// check a user without permissions and with roles
		final SimplePrincipalCollection edPr = new SimplePrincipalCollection(
				"eddie", realm.getName());
		realm.addUser("eddie", "password", new String[] { "aRole" }, null);

		roles = realm.getUserRoles("eddie");
		assertEquals(1, roles.size());
		assertTrue(roles.contains("aRole"));

		final AuthorizationInfo edInfo = realm.doGetAuthorizationInfo(edPr);
		assertNotNull(edInfo);
		assertNull(edInfo.getObjectPermissions());
		assertEquals(1, edInfo.getRoles().size());

		// check a user with permissions and with roles
		final SimplePrincipalCollection dePr = new SimplePrincipalCollection(
				"debbie", realm.getName());
		realm.addUser("debbie", "password", new String[] { "aRole",
				"anotherRole" }, new String[] { "cando", "notcando" });

		roles = realm.getUserRoles("debbie");
		assertEquals(2, roles.size());
		assertTrue(roles.contains("aRole"));
		assertTrue(roles.contains("anotherRole"));

		final AuthorizationInfo deInfo = realm.doGetAuthorizationInfo(dePr);
		assertNotNull(deInfo);
		assertEquals(2, deInfo.getObjectPermissions().size());
		assertEquals(2, deInfo.getRoles().size());

		// check the users
		final Set<String> users = realm.getUsers();
		assertEquals(5, users.size());
		assertTrue(users.contains("admin"));
		assertTrue(users.contains("philipp"));
		assertTrue(users.contains("tobias"));
		assertTrue(users.contains("eddie"));
		assertTrue(users.contains("debbie"));

		// cleanUp
		realm.release();
	}

	/**
	 * Tests the exception to be thrown if a user with a name already available
	 * is added.
	 */
	@Test
	public void testAddUserDuplicate() {
		final MapDbAuthorizingRealm realm = new MapDbAuthorizingRealm(
				realmDbDir);
		realm.init();

		// add a user
		realm.addUser("philipp", "password", null, null);

		// add the same user
		thrown.expect(ForwardedRuntimeException.class);
		thrown.expect(new ForwardExceptionMatcher(
				AuthManagementException.class, 1000));
		realm.addUser("philipp", "diffPassword", new String[] { "one" }, null);
	}

	/**
	 * Tests the deletion of users.
	 */
	@Test
	public void testDeleteUser() {
		final MapDbAuthorizingRealm realm = new MapDbAuthorizingRealm(
				realmDbDir);
		realm.init();

		final UsernamePasswordToken phTo = new UsernamePasswordToken("philipp",
				"password");

		// delete a user which isn't in the realm yet
		assertNull(realm.doGetAuthenticationInfo(phTo));
		realm.deleteUser("philipp");
		assertNull(realm.doGetAuthenticationInfo(phTo));

		// add a user
		realm.addUser("philipp", "password", null, null);
		assertNotNull(realm.doGetAuthenticationInfo(phTo));

		// and remove it
		realm.deleteUser("philipp");
		assertNull(realm.doGetAuthenticationInfo(phTo));

		// cleanUp
		realm.release();
	}

	/**
	 * Tests the modification of a password of a user.
	 */
	@Test
	public void testPasswordModification() {
		final MapDbAuthorizingRealm realm = new MapDbAuthorizingRealm(
				realmDbDir);
		realm.init();

		AuthenticationInfo info;
		final UsernamePasswordToken phTo = new UsernamePasswordToken("philipp",
				"password");

		// add a user
		realm.addUser("philipp", "password", null, null);
		info = realm.doGetAuthenticationInfo(phTo);
		assertEquals("password", info.getCredentials());

		// change the password and check
		realm.modifyPassword("philipp", "changedPassword");
		info = realm.doGetAuthenticationInfo(phTo);
		assertEquals("changedPassword", info.getCredentials());

		// cleanUp
		realm.release();
	}

	/**
	 * Tests the exception to be thrown if an unknown user's password is
	 * modified.
	 */
	@Test
	public void testPasswordModificationOfUnknownUser() {
		final MapDbAuthorizingRealm realm = new MapDbAuthorizingRealm(
				realmDbDir);
		realm.init();

		// change the password of an unknown user
		thrown.expect(ForwardedRuntimeException.class);
		thrown.expect(new ForwardExceptionMatcher(
				AuthManagementException.class, 1001));
		realm.modifyPassword("philipp", "changedPassword");
	}

	/**
	 * Tests the adding of a role.
	 */
	@Test
	public void testAddRole() {
		final MapDbAuthorizingRealm realm = new MapDbAuthorizingRealm(
				realmDbDir);
		realm.init();

		Collection<Permission> perms;
		perms = realm.getRolePermissionResolver().resolvePermissionsInRole(
				"loader");
		assertNull(perms);

		// check the added permission and role using the resolver
		realm.addRole("loader", new String[] { "load" });
		perms = realm.getRolePermissionResolver().resolvePermissionsInRole(
				"loader");
		assertEquals(1, perms.size());

		// cleanUp
		realm.release();
	}

	/**
	 * Tests the exception to be thrown if a role is added, with a name which
	 * already exists.
	 */
	@Test
	public void testAddRoleDuplicateException() {
		final MapDbAuthorizingRealm realm = new MapDbAuthorizingRealm(
				realmDbDir);
		realm.init();

		// check the added permission and role using the resolver
		realm.addRole("loader", new String[] { "load" });

		thrown.expect(ForwardedRuntimeException.class);
		thrown.expect(new ForwardExceptionMatcher(
				AuthManagementException.class, 1002));
		realm.addRole("loader", null);
	}

	/**
	 * Tests the deletion of users.
	 */
	@Test
	public void testDeleteRole() {
		final MapDbAuthorizingRealm realm = new MapDbAuthorizingRealm(
				realmDbDir);
		realm.init();

		// make sure there is no role
		Collection<Permission> perms;
		perms = realm.getRolePermissionResolver().resolvePermissionsInRole(
				"theOne");
		assertNull(perms);

		// delete the role
		realm.deleteRole("theOne");
		perms = realm.getRolePermissionResolver().resolvePermissionsInRole(
				"theOne");
		assertNull(perms);

		// check the added permission and role using the resolver
		realm.addRole("theOne", new String[] { "aRing" });
		perms = realm.getRolePermissionResolver().resolvePermissionsInRole(
				"theOne");
		assertEquals(1, perms.size());

		// and remove it
		realm.deleteRole("theOne");
		perms = realm.getRolePermissionResolver().resolvePermissionsInRole(
				"theOne");
		assertNull(perms);

		// cleanUp
		realm.release();
	}

	/**
	 * Tests the {@link MapDbAuthorizingRealm#assignRoleToUser(String, String)}
	 * implementation.
	 */
	@Test
	public void testAssignRoleToUser() {
		final MapDbAuthorizingRealm realm = new MapDbAuthorizingRealm(
				realmDbDir);
		realm.init();

		AuthorizationInfo edInfo;

		// check a user without permissions and with roles
		final SimplePrincipalCollection edPr = new SimplePrincipalCollection(
				"eddie", realm.getName());
		realm.addUser("eddie", "password", null, null);

		// check the created user
		edInfo = realm.doGetAuthorizationInfo(edPr);
		assertNotNull(edInfo);
		assertNull(edInfo.getRoles());
		assertNull(edInfo.getObjectPermissions());

		// assign a role to the user
		realm.assignRoleToUser("eddie", "aRole");
		edInfo = realm.doGetAuthorizationInfo(edPr);
		assertNotNull(edInfo);
		assertNotNull(edInfo.getRoles());
		assertEquals(1, edInfo.getRoles().size());

		// check the same assignment
		realm.assignRoleToUser("eddie", "aRole");
		edInfo = realm.doGetAuthorizationInfo(edPr);
		assertNotNull(edInfo);
		assertNotNull(edInfo.getRoles());
		assertEquals(1, edInfo.getRoles().size());

		// add another role
		realm.assignRoleToUser("eddie", "secondRole");
		edInfo = realm.doGetAuthorizationInfo(edPr);
		assertNotNull(edInfo);
		assertNotNull(edInfo.getRoles());
		assertEquals(2, edInfo.getRoles().size());

		// cleanUp
		realm.release();
	}

	/**
	 * Tests the exception to be thrown if a role is assigned to a user, which
	 * does not exist.
	 */
	@Test
	public void testAssignRoleToUserNoUserException() {
		final MapDbAuthorizingRealm realm = new MapDbAuthorizingRealm(
				realmDbDir);
		realm.init();

		// tests the exception to be thrown
		thrown.expect(ForwardedRuntimeException.class);
		thrown.expect(new ForwardExceptionMatcher(
				AuthManagementException.class, 1004));
		realm.assignRoleToUser("notExists", "aRole");
	}

	/**
	 * Tests the
	 * {@link MapDbAuthorizingRealm#removeRoleFromUser(String, String)}
	 * implementation.
	 */
	@Test
	public void testRemoveRoleFromUser() {
		Set<String> perms;

		final MapDbAuthorizingRealm realm = new MapDbAuthorizingRealm(
				realmDbDir);
		realm.init();

		AuthorizationInfo edInfo;

		// check a user without permissions and with roles
		final SimplePrincipalCollection edPr = new SimplePrincipalCollection(
				"eddie", realm.getName());
		realm.addUser("eddie", "password", null, null);

		// check the created user
		edInfo = realm.doGetAuthorizationInfo(edPr);
		assertNotNull(edInfo);
		assertNull(edInfo.getRoles());
		assertNull(edInfo.getObjectPermissions());

		perms = realm.getUserPermissions("eddie",
				ShiroAuthManager.permissionSeparator);
		assertEquals(0, perms.size());

		// assign a role to the user
		realm.assignRoleToUser("eddie", "aRole");
		edInfo = realm.doGetAuthorizationInfo(edPr);
		assertNotNull(edInfo);
		assertNotNull(edInfo.getRoles());
		assertEquals(1, edInfo.getRoles().size());

		perms = realm.getUserPermissions("eddie",
				ShiroAuthManager.permissionSeparator);
		assertEquals(perms.toString(), 0, perms.size());

		// remove it
		realm.removeRoleFromUser("eddie", "aRole");
		edInfo = realm.doGetAuthorizationInfo(edPr);
		assertNotNull(edInfo);
		assertNull(edInfo.getRoles());
		assertNull(edInfo.getObjectPermissions());

		// cleanUp
		realm.release();
	}

	/**
	 * Tests the exception to be thrown, if a role is removed from a
	 * non-existing user.
	 */
	@Test
	public void testRemoveRoleFromUserNoUserException() {
		final MapDbAuthorizingRealm realm = new MapDbAuthorizingRealm(
				realmDbDir);
		realm.init();

		// tests the exception to be thrown
		thrown.expect(ForwardedRuntimeException.class);
		thrown.expect(new ForwardExceptionMatcher(
				AuthManagementException.class, 1005));
		realm.removeRoleFromUser("notExists", "aRole");
	}

	/**
	 * Tests the granting of the permissions to a user, i.e.
	 * {@link MapDbAuthorizingRealm#grantPermissionsToUser(String, String[])}.
	 */
	@Test
	public void testGrantPermissionsToUser() {
		final MapDbAuthorizingRealm realm = new MapDbAuthorizingRealm(
				realmDbDir);
		realm.init();

		AuthorizationInfo edInfo;

		// check a user without permissions and with roles
		final SimplePrincipalCollection edPr = new SimplePrincipalCollection(
				"eddie", realm.getName());
		realm.addUser("eddie", "password", null, null);
		edInfo = realm.doGetAuthorizationInfo(edPr);
		assertNull(edInfo.getObjectPermissions());

		// grant the permissions and check
		final String[] perms = net.meisen.dissertation.model.auth.permissions.Permission
				.transform(
						new net.meisen.dissertation.model.auth.permissions.Permission[] {
								net.meisen.dissertation.model.auth.permissions.Permission.connectHTTP,
								net.meisen.dissertation.model.auth.permissions.Permission.modify },
						"myModel", ShiroAuthManager.permissionSeparator);
		realm.grantPermissionsToUser("eddie", perms);
		edInfo = realm.doGetAuthorizationInfo(edPr);
		assertNotNull(edInfo.getObjectPermissions());
		assertEquals(2, edInfo.getObjectPermissions().size());

		final Set<String> userPerms = realm.getUserPermissions("eddie",
				ShiroAuthManager.permissionSeparator);
		assertEquals(2, userPerms.size());

		assertTrue(userPerms.toString() + " contains " + perms[0],
				userPerms.contains(perms[0]));

		assertTrue(userPerms.toString() + " contains " + perms[1],
				userPerms.contains(perms[1]));

		// cleanUp
		realm.release();
	}

	/**
	 * Tests the exception to be thrown if permissions are granted to a
	 * non-existing user.
	 */
	@Test
	public void testGrantPermissionsToUserNoUserException() {
		final MapDbAuthorizingRealm realm = new MapDbAuthorizingRealm(
				realmDbDir);
		realm.init();

		// tests the exception to be thrown
		thrown.expect(ForwardedRuntimeException.class);
		thrown.expect(new ForwardExceptionMatcher(
				AuthManagementException.class, 1009));
		realm.grantPermissionsToUser("notExists", new String[] { "permission" });
	}

	/**
	 * Tests the exception to be thrown if someone tries to add empty
	 * permissions to the user.
	 */
	@Test
	public void testGrantPermissionsToUserNoPermissionsException() {
		final MapDbAuthorizingRealm realm = new MapDbAuthorizingRealm(
				realmDbDir);
		realm.init();

		// tests the exception to be thrown
		thrown.expect(ForwardedRuntimeException.class);
		thrown.expect(new ForwardExceptionMatcher(
				AuthManagementException.class, 1008));
		realm.grantPermissionsToUser("notExists", new String[] {});
	}

	/**
	 * Tests the implementation of
	 * {@link MapDbAuthorizingRealm#revokePermissionsFromUser(String, String[])}
	 * .
	 */
	@Test
	public void testRevokePermissionsFromUser() {
		final String sep = ShiroAuthManager.permissionSeparator;

		final MapDbAuthorizingRealm realm = new MapDbAuthorizingRealm(
				realmDbDir);
		realm.init();

		final DefinedPermission[] perms = new DefinedPermission[] {
				net.meisen.dissertation.model.auth.permissions.Permission.connectHTTP
						.create(),
				net.meisen.dissertation.model.auth.permissions.Permission.query
						.create("myModel"),
				net.meisen.dissertation.model.auth.permissions.Permission.grantPermissions
						.create() };

		realm.addUser("philipp", "password", null,
				net.meisen.dissertation.model.auth.permissions.Permission
						.transform(perms, sep));

		final SimplePrincipalCollection phPr = new SimplePrincipalCollection(
				"philipp", realm.getName());
		assertTrue(realm.isPermitted(phPr, perms[0].toString(sep)));
		assertTrue(realm.isPermitted(phPr, perms[1].toString(sep)));
		assertTrue(realm.isPermitted(phPr, perms[2].toString(sep)));

		// revoke a permission which does not exist
		realm.revokePermissionsFromUser(
				"philipp",
				new String[] { net.meisen.dissertation.model.auth.permissions.Permission.connectTSQL
						.create().toString() });
		assertTrue(realm.isPermitted(phPr, perms[0].toString(sep)));
		assertTrue(realm.isPermitted(phPr, perms[1].toString(sep)));
		assertTrue(realm.isPermitted(phPr, perms[2].toString(sep)));

		// revoke one permission
		realm.revokePermissionsFromUser("philipp",
				new String[] { perms[0].toString(sep) });
		assertFalse(realm.isPermitted(phPr, perms[0].toString(sep)));
		assertTrue(realm.isPermitted(phPr, perms[1].toString(sep)));
		assertTrue(realm.isPermitted(phPr, perms[2].toString(sep)));

		final Set<String> userPerms = realm.getUserPermissions("philipp",
				ShiroAuthManager.permissionSeparator);
		assertEquals(2, userPerms.size());
		assertTrue(userPerms.toString(),
				userPerms.contains(perms[1].toString(sep)));
		assertTrue(userPerms.toString(),
				userPerms.contains(perms[2].toString(sep)));

		// revoke it again permission
		realm.revokePermissionsFromUser("philipp",
				new String[] { perms[0].toString(sep) });
		assertFalse(realm.isPermitted(phPr, perms[0].toString(sep)));
		assertTrue(realm.isPermitted(phPr, perms[1].toString(sep)));
		assertTrue(realm.isPermitted(phPr, perms[2].toString(sep)));

		// revoke all
		realm.revokePermissionsFromUser("philipp",
				new String[] { perms[1].toString(sep), perms[2].toString(sep),
						"dontexist" });
		assertFalse(realm.isPermitted(phPr, perms[0].toString(sep)));
		assertFalse(realm.isPermitted(phPr, perms[1].toString(sep)));
		assertFalse(realm.isPermitted(phPr, perms[2].toString(sep)));

		// cleanUp
		realm.release();
	}

	/**
	 * Tests the exception to be thrown if the user to revoke the permissions
	 * from does not exist.
	 */
	@Test
	public void testRevokePermissionsFromUserNoUserException() {
		final MapDbAuthorizingRealm realm = new MapDbAuthorizingRealm(
				realmDbDir);
		realm.init();

		// tests the exception to be thrown
		thrown.expect(ForwardedRuntimeException.class);
		thrown.expect(new ForwardExceptionMatcher(
				AuthManagementException.class, 1010));
		realm.revokePermissionsFromUser("notExists",
				new String[] { "permission" });
	}

	/**
	 * Tests the implementation of
	 * {@link MapDbAuthorizingRealm#grantPermissionsToRole(String, String[])}.
	 */
	@Test
	public void testGrantPermissionsToRole() {
		Collection<Permission> perms;

		final MapDbAuthorizingRealm realm = new MapDbAuthorizingRealm(
				realmDbDir);
		realm.init();

		realm.addRole("role", new String[] { "perm1", "perm2", "perm3" });
		perms = realm.getRolePermissionResolver().resolvePermissionsInRole(
				"role");
		assertEquals(3, perms.size());
		assertTrue(realm.isPermitted("role", "perm1"));
		assertTrue(realm.isPermitted("role", "perm2"));
		assertTrue(realm.isPermitted("role", "perm3"));

		// grant the same permissions again
		realm.grantPermissionsToRole("role", new String[] { "perm1", "perm2",
				"perm3" });
		perms = realm.getRolePermissionResolver().resolvePermissionsInRole(
				"role");
		assertEquals(3, perms.size());
		assertTrue(realm.isPermitted("role", "perm1"));
		assertTrue(realm.isPermitted("role", "perm2"));
		assertTrue(realm.isPermitted("role", "perm3"));

		// grant new once
		realm.grantPermissionsToRole("role", new String[] { "perm4", "perm2",
				"perm3" });
		perms = realm.getRolePermissionResolver().resolvePermissionsInRole(
				"role");
		assertEquals(4, perms.size());
		assertTrue(realm.isPermitted("role", "perm1"));
		assertTrue(realm.isPermitted("role", "perm2"));
		assertTrue(realm.isPermitted("role", "perm3"));
		assertTrue(realm.isPermitted("role", "perm4"));

		// cleanUp
		realm.release();
	}

	/**
	 * Tests the exception to be thrown if the role to revoke the permissions
	 * from does not exist.
	 */
	@Test
	public void testGrantPermissionsToRoleNoRoleException() {
		final MapDbAuthorizingRealm realm = new MapDbAuthorizingRealm(
				realmDbDir);
		realm.init();

		// tests the exception to be thrown
		thrown.expect(ForwardedRuntimeException.class);
		thrown.expect(new ForwardExceptionMatcher(
				AuthManagementException.class, 1011));
		realm.grantPermissionsToRole("notExists", new String[] { "permission" });
	}

	/**
	 * Tests the exception to be thrown if someone tries to add empty
	 * permissions to the role.
	 */
	@Test
	public void testGrantPermissionsToRoleNoPermissionsException() {
		final MapDbAuthorizingRealm realm = new MapDbAuthorizingRealm(
				realmDbDir);
		realm.init();

		// tests the exception to be thrown
		thrown.expect(ForwardedRuntimeException.class);
		thrown.expect(new ForwardExceptionMatcher(
				AuthManagementException.class, 1013));
		realm.grantPermissionsToRole("notExists", new String[] {});
	}

	/**
	 * Tests the implementation of
	 * {@link MapDbAuthorizingRealm#revokePermissionsFromRole(String, String[])}
	 * .
	 */
	@Test
	public void testRevokePermissionsFromRole() {
		Collection<Permission> perms;

		final MapDbAuthorizingRealm realm = new MapDbAuthorizingRealm(
				realmDbDir);
		realm.init();

		realm.addRole("role", new String[] { "perm1", "perm2", "perm3" });
		perms = realm.getRolePermissionResolver().resolvePermissionsInRole(
				"role");
		assertEquals(3, perms.size());
		assertTrue(realm.isPermitted("role", "perm1"));
		assertTrue(realm.isPermitted("role", "perm2"));
		assertTrue(realm.isPermitted("role", "perm3"));

		// revoke a permission which does not exist
		realm.revokePermissionsFromRole("role", new String[] { "perm4" });
		perms = realm.getRolePermissionResolver().resolvePermissionsInRole(
				"role");
		assertEquals(3, perms.size());
		assertTrue(realm.isPermitted("role", "perm1"));
		assertTrue(realm.isPermitted("role", "perm2"));
		assertTrue(realm.isPermitted("role", "perm3"));

		// revoke a single permission
		realm.revokePermissionsFromRole("role", new String[] { "perm1" });
		perms = realm.getRolePermissionResolver().resolvePermissionsInRole(
				"role");
		assertEquals(2, perms.size());
		assertFalse(realm.isPermitted("role", "perm1"));
		assertTrue(realm.isPermitted("role", "perm2"));
		assertTrue(realm.isPermitted("role", "perm3"));

		// revoke all permission
		realm.revokePermissionsFromRole("role", new String[] { "perm2",
				"perm3", "perm4" });
		perms = realm.getRolePermissionResolver().resolvePermissionsInRole(
				"role");
		assertEquals(0, perms.size());
		assertFalse(realm.isPermitted("role", "perm1"));
		assertFalse(realm.isPermitted("role", "perm2"));
		assertFalse(realm.isPermitted("role", "perm3"));

		// cleanUp
		realm.release();
	}

	/**
	 * Tests the exception to be thrown if a permission is revoked from a role,
	 * which does not exist.
	 */
	@Test
	public void testRevokePermissionsFromRoleNoRoleException() {
		final MapDbAuthorizingRealm realm = new MapDbAuthorizingRealm(
				realmDbDir);
		realm.init();

		// tests the exception to be thrown
		thrown.expect(ForwardedRuntimeException.class);
		thrown.expect(new ForwardExceptionMatcher(
				AuthManagementException.class, 1012));
		realm.revokePermissionsFromRole("notExists",
				new String[] { "permission" });
	}

	/**
	 * Tests the cloning of a {@code SimpleAccount}.
	 * 
	 * @see SimpleAccount
	 * @see MapDbAuthorizingRealm#clone(SimpleAccount)
	 */
	@Test
	public void testClone() {
		SimpleAccount account;

		final MapDbAuthorizingRealm realm = new MapDbAuthorizingRealm(
				realmDbDir);
		realm.init();

		// null clone
		assertNull(realm.clone((SimpleAccount) null));

		// simple clone
		account = new SimpleAccount("eddie", "mine", "realm");
		assertEquals("eddie", account.getPrincipals().fromRealm("realm")
				.iterator().next());
		assertEquals("mine", account.getCredentials());
		assertNull(account.getObjectPermissions());
		assertNull(account.getRoles());

		account = realm.clone(account);
		assertEquals("eddie", account.getPrincipals().fromRealm("realm")
				.iterator().next());
		assertEquals("mine", account.getCredentials());
		assertNull(account.getObjectPermissions());
		assertNull(account.getRoles());

		// cleanUp
		realm.release();
	}

	/**
	 * Simply tests a reloading.
	 */
	@Test
	public void testReloading() {
		final MapDbAuthorizingRealm realm = new MapDbAuthorizingRealm(
				realmDbDir);
		realm.init();

		realm.addRole("fighter", new String[] { "createMess" });
		realm.addRole("loader", new String[] { "getAmmo" });
		realm.addUser("philipp", "password",
				new String[] { "fighter", "loader" },
				new String[] { "cleanUp" });

		assertFalse(realm.isClosed());
		realm.release();
		assertTrue(realm.isClosed());

		// initialize it again using the same dir
		assertTrue(realm.isClosed());
		realm.init();
		assertFalse(realm.isClosed());

		final UsernamePasswordToken phTo = new UsernamePasswordToken("philipp",
				"password");
		final SimplePrincipalCollection phPr = new SimplePrincipalCollection(
				"philipp", realm.getName());

		final AuthenticationInfo phInfo1 = realm.doGetAuthenticationInfo(phTo);
		final AuthorizationInfo phInfo2 = realm.doGetAuthorizationInfo(phPr);
		assertNotNull(phInfo1);
		assertNotNull(phInfo2);

		// check the users
		assertEquals("password", phInfo1.getCredentials());
		assertEquals(1, phInfo2.getObjectPermissions().size());

		// check the roles
		assertTrue(realm.hasRole(phPr, "fighter"));
		assertTrue(realm.hasRole(phPr, "loader"));
	}

	/**
	 * Cleans up behind the test.
	 */
	@After
	public void cleanUp() {
		Files.deleteOnExitDir(realmDbDir);
	}
}
