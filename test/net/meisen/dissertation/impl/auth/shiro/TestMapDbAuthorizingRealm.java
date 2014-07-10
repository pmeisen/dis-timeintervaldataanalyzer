package net.meisen.dissertation.impl.auth.shiro;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;

import java.io.File;
import java.util.UUID;

import net.meisen.dissertation.exceptions.AuthManagementException;
import net.meisen.dissertation.exceptions.GroupEvaluatorException;
import net.meisen.dissertation.help.ExceptionBasedTest;
import net.meisen.dissertation.help.ExceptionBasedTest.ForwardExceptionMatcher;
import net.meisen.general.genmisc.exceptions.ForwardedRuntimeException;
import net.meisen.general.genmisc.types.Files;

import org.apache.shiro.authc.AuthenticationInfo;
import org.apache.shiro.authc.UsernamePasswordToken;
import org.apache.shiro.authz.AuthorizationInfo;
import org.apache.shiro.subject.SimplePrincipalCollection;
import org.hamcrest.Description;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;
import org.junit.internal.matchers.TypeSafeMatcher;

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
		realm.destroy();
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

		final AuthorizationInfo edInfo = realm.doGetAuthorizationInfo(edPr);
		assertNotNull(edInfo);
		assertNull(edInfo.getObjectPermissions());
		assertEquals(1, edInfo.getRoles().size());

		// check a user with permissions and with roles
		final SimplePrincipalCollection dePr = new SimplePrincipalCollection(
				"debbie", realm.getName());
		realm.addUser("debbie", "password", new String[] { "aRole",
				"anotherRole" }, new String[] { "cando", "notcando" });

		final AuthorizationInfo deInfo = realm.doGetAuthorizationInfo(dePr);
		assertNotNull(deInfo);
		assertEquals(2, deInfo.getObjectPermissions().size());
		assertEquals(2, deInfo.getRoles().size());

		// cleanUp
		realm.destroy();
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
		realm.destroy();
	}

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
		realm.destroy();
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

	@Test
	public void testAddRole() {
		final MapDbAuthorizingRealm realm = new MapDbAuthorizingRealm(
				realmDbDir);
		realm.init();

		AuthorizationInfo info;
		final SimplePrincipalCollection phPr = new SimplePrincipalCollection(
				"philipp", realm.getName());

		realm.addRole("loader", new String[] { "load" });
		realm.addUser("philipp", "password", new String[] { "loader " }, null);
		info = realm.doGetAuthorizationInfo(phPr);

	}

	/**
	 * Cleans up behind the test.
	 */
	@After
	public void cleanUp() {
		Files.deleteOnExitDir(realmDbDir);
	}
}
