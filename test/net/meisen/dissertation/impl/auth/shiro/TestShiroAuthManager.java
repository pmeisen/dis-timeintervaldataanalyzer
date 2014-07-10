package net.meisen.dissertation.impl.auth.shiro;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import java.io.File;

import net.meisen.dissertation.config.xslt.DefaultValues;
import net.meisen.dissertation.exceptions.AuthException;
import net.meisen.dissertation.exceptions.PermissionException;
import net.meisen.dissertation.help.LoaderBasedTest;
import net.meisen.dissertation.model.auth.permissions.DefinedPermission;
import net.meisen.dissertation.model.auth.permissions.Permission;
import net.meisen.dissertation.model.handler.TidaModelHandler;
import net.meisen.general.genmisc.types.Files;
import net.meisen.general.sbconfigurator.runners.annotations.SystemProperties;
import net.meisen.general.sbconfigurator.runners.annotations.SystemProperty;

import org.junit.After;
import org.junit.AfterClass;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;

/**
 * Tests the implementation of the Shiro based {@code ShiroAuthManager}.
 * 
 * @author pmeisen
 * 
 */
@SystemProperties(value = {
		@SystemProperty(property = "tida.config.selector", value = "net/meisen/dissertation/impl/auth/shiro/tidaConfigUsingShiroAuth.xml"),
		@SystemProperty(property = "test.tmpFolder", value = "testShiroAuthManager") })
public class TestShiroAuthManager extends LoaderBasedTest {
	private static TidaModelHandler h = null;

	@Autowired
	@Qualifier(DefaultValues.HANDLER_ID)
	private TidaModelHandler handler;

	@Autowired
	@Qualifier(DefaultValues.AUTHMANAGER_ID)
	private ShiroAuthManager authManager;

	/**
	 * Initializes the tests by setting the static handler {@code h}.
	 */
	@Before
	public void set() {

		if (h == null) {
			h = handler;
		} else {
			assertTrue(h == handler);
		}
	}

	/**
	 * Tests the usage of an invalid user during login.
	 */
	@Test
	public void testInvalidLogin() {
		thrown.expect(AuthException.class);
		thrown.expectMessage("Invalid user credentials for user 'philipp'");
		authManager.login("philipp", "password");
	}

	/**
	 * Tests the usage of an invalid credentials during login.
	 */
	@Test
	public void testInvalidCredential() {
		authManager.login(MapDbAuthorizingRealm.adminName,
				MapDbAuthorizingRealm.adminPassword);
		authManager.addUser("philipp", "password", null, null);
		authManager.logout();

		thrown.expect(AuthException.class);
		thrown.expectMessage("Invalid user credentials for user 'philipp'");
		authManager.login("philipp", "password2");
	}

	@Test
	public void testInsufficientPermissionToAddUser() {
		thrown.expect(PermissionException.class);
		thrown.expectMessage("Insufficient permission '"
				+ Permission.manageUsers.toString() + "'");
		authManager.addUser("philipp", "password", null, null);
	}

	@Test
	public void testInsufficientPermissionToDeleteUser() {
		thrown.expect(PermissionException.class);
		thrown.expectMessage("Insufficient permission '"
				+ Permission.manageUsers.toString() + "'");
		authManager.deleteUser("philipp");
	}

	@Test
	public void testInsufficientPermissionToModifyUser() {
		thrown.expect(PermissionException.class);
		thrown.expectMessage("Insufficient permission '"
				+ Permission.manageUsers.toString() + "'");
		authManager.modifyPassword("philipp", "password");
	}

	@Test
	public void testInsufficientPermissionToAddRole() {
		thrown.expect(PermissionException.class);
		thrown.expectMessage("Insufficient permission '"
				+ Permission.manageUsers.toString() + "'");
		authManager.addRole("myRole", null);
	}

	@Test
	public void testInsufficientPermissionToDeleteRole() {
		thrown.expect(PermissionException.class);
		thrown.expectMessage("Insufficient permission '"
				+ Permission.manageUsers.toString() + "'");
		authManager.deleteRole("myRole");
	}

	@Test
	public void testInsufficientPermissionToAssignRoleToUser() {
		thrown.expect(PermissionException.class);
		thrown.expectMessage("Insufficient permission '"
				+ Permission.manageUsers.toString() + "'");
		authManager.assignRoleToUser("myRole", "philipp");
	}

	@Test
	public void testInsufficientPermissionToRemoveRoleFromUser() {
		thrown.expect(PermissionException.class);
		thrown.expectMessage("Insufficient permission '"
				+ Permission.manageUsers.toString() + "'");
		authManager.removeRoleFromUser("myRole", "philipp");
	}

	@Test
	public void testInsufficientPermissionToGrantPermissionsToUser() {
		thrown.expect(PermissionException.class);
		thrown.expectMessage("Insufficient permission '"
				+ Permission.manageUsers.toString() + "'");
		authManager.grantPermissionsToUser("philipp", null);
	}

	@Test
	public void testInsufficientPermissionToRevokePermissionsFromUser() {
		thrown.expect(PermissionException.class);
		thrown.expectMessage("Insufficient permission '"
				+ Permission.manageUsers.toString() + "'");
		authManager.revokePermissionsFromUser("philipp", null);
	}

	@Test
	public void testInsufficientPermissionToGrantPermissionsToRole() {
		thrown.expect(PermissionException.class);
		thrown.expectMessage("Insufficient permission '"
				+ Permission.manageUsers.toString() + "'");
		authManager.grantPermissionsToRole("myRole", null);
	}

	@Test
	public void testInsufficientPermissionToRevokePermissionsFromRole() {
		thrown.expect(PermissionException.class);
		thrown.expectMessage("Insufficient permission '"
				+ Permission.manageUsers.toString() + "'");
		authManager.revokePermissionsFromRole("myRole", null);
	}

	/**
	 * Tests a valid login and a user creation.
	 */
	@Test
	public void testValidLogin() {
		authManager.login(MapDbAuthorizingRealm.adminName,
				MapDbAuthorizingRealm.adminPassword);
		authManager.addUser("philipp", "password", null, null);
		authManager.logout();

		authManager.login("philipp", "password");
		assertTrue(authManager.getSubject().isAuthenticated());

		authManager.logout();
		assertFalse(authManager.getSubject().isAuthenticated());
	}

	/**
	 * Tests a login after the user is deleted.
	 */
	@Test
	public void testInvalidLoginAfterUserDeletion() {
		authManager.login(MapDbAuthorizingRealm.adminName,
				MapDbAuthorizingRealm.adminPassword);
		authManager.addUser("philipp", "password", null,
				new DefinedPermission[] { Permission.manageUsers.create() });
		authManager.logout();

		// login as the created user
		authManager.login("philipp", "password");
		assertTrue(authManager.getSubject().isAuthenticated());
		authManager.deleteUser("philipp");
		assertTrue(authManager.getSubject().isAuthenticated());

		// now logout
		authManager.logout();

		// login again
		thrown.expect(AuthException.class);
		thrown.expectMessage("Invalid user credentials for user 'philipp'");
		authManager.login("philipp", "password");
	}

	/**
	 * Tests the exception to be thrown if an attempt to log in is made while
	 * another user is logged in.
	 */
	@Test
	public void testDuplicateLogin() {
		authManager.login(MapDbAuthorizingRealm.adminName,
				MapDbAuthorizingRealm.adminPassword);
		authManager.addUser("philipp", "password", null,
				new DefinedPermission[] { Permission.manageUsers.create() });

		thrown.expect(AuthException.class);
		thrown.expectMessage("Another user '" + MapDbAuthorizingRealm.adminName
				+ "' is currently connected");
		authManager.login("philipp", "password");
	}

	/**
	 * Remove everything from the {@code AuthManager}, which might have been
	 * added during the test.
	 */
	@After
	public void cleanUp() {
		authManager.logout();

		// remove everything modified during the test
		authManager.clear();
	}

	/**
	 * Cleans up by trying to delete created files.
	 */
	@BeforeClass
	@AfterClass
	public static void cleanup() {
		if (h == null) {
			Files.deleteOnExitDir(new File(
					System.getProperty("java.io.tmpdir"),
					"testShiroAuthManager"));
		} else {
			h.unloadAll();
			Files.deleteOnExitDir(new File(h.getDefaultLocation()));
		}
	}
}
