package net.meisen.dissertation.server;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;

import java.io.UnsupportedEncodingException;
import java.sql.SQLException;
import java.sql.Statement;
import java.util.HashMap;
import java.util.Map;

import org.junit.Before;
import org.junit.Test;

import com.eclipsesource.json.JsonObject;

/**
 * Tests the implementation of the {@code AuthServlet}.
 * 
 * @author pmeisen
 * 
 */
public class TestAuthServlet extends BaseTestWithServerConnection {

	@Override
	public String getConfig() {
		return "net/meisen/dissertation/server/testShiroAuthConfig.xml";
	}

	@Override
	public boolean isHttp() {
		return true;
	}

	@Override
	public boolean isTSQL() {
		return true;
	}

	/**
	 * Make sure we have some users, so that we can test the authentication.
	 * 
	 * @throws SQLException
	 *             if the users cannot be created
	 */
	@Before
	public void addUser() throws SQLException {
		Statement stmt;

		// add some users and roles
		stmt = conn.createStatement();
		stmt.executeUpdate("ADD USER 'eddie'   WITH PASSWORD 'password'");
		stmt.executeUpdate("ADD USER 'philipp' WITH PASSWORD 'password' WITH ROLES 'connect', 'superuser' WITH PERMISSIONS 'MODEL.testNumberModel.modify'");
		stmt.executeUpdate("ADD USER 'tobias'  WITH PASSWORD 'password' WITH ROLES 'readOnlyNumberModel', 'connect'");

		stmt.executeUpdate("ADD ROLE 'readOnlyNumberModel' WITH PERMISSIONS 'MODEL.testNumberModel.query'");
		stmt.executeUpdate("ADD ROLE 'connect'             WITH PERMISSIONS 'GLOBAL.connectHTTP'");
		stmt.executeUpdate("ADD ROLE 'superuser'           WITH PERMISSIONS 'GLOBAL.load', 'GLOBAL.get', 'GLOBAL.queryAll', 'GLOBAL.modifyAll'");
		stmt.close();
	}

	/**
	 * Tests the exception to be thrown if the user tries to login twice.
	 * 
	 * @throws UnsupportedEncodingException
	 *             if the encoding is not supported
	 */
	@Test
	public void testDoubleAuthentication() throws UnsupportedEncodingException {

		final Map<String, String> params = new HashMap<String, String>();
		params.put("username", "philipp");
		params.put("password", "password");
		final byte[] response = this.getResponse("/auth/login", params);

		// make sure we got a sessionId
		JsonObject value;
		value = JsonObject.readFrom(new String(response, "UTF8"));
		assertNotNull(value);

		// let's try to login again, should lead to an exception
		params.clear();
		params.put("sessionId", value.get("sessionId").asString());
		value = JsonObject.readFrom(new String(getResponse("/auth/login",
				params)));
		assertNotNull(value);
		assertEquals("error", value.get("type").asString());
		assertEquals(
				"Another user 'philipp' is currently connected, please perform a logout prior to relogin.",
				value.get("message").asString());

		this.getResponse("/auth/logout", params);
	}

	/**
	 * Tests the retrieval of user information.
	 * 
	 * @throws UnsupportedEncodingException
	 *             if the retrieved data is of an invalid encoding
	 */
	@Test
	public void testUserInfoAuthentication()
			throws UnsupportedEncodingException {

		final Map<String, String> params = new HashMap<String, String>();
		params.put("username", "philipp");
		params.put("password", "password");

		// make sure we got a sessionId
		JsonObject value;
		value = JsonObject.readFrom(new String(getResponse("/auth/login",
				params), "UTF8"));
		assertNotNull(value);

		// let's try to login again, should lead to an exception
		params.clear();
		params.put("sessionId", value.get("sessionId").asString());
		value = JsonObject.readFrom(new String(getResponse("/auth/userinfo",
				params)));
		assertNotNull(value);
		assertNotNull(value.get("sessionId").asString());
		assertEquals(value.get("username").asString(), "philipp");

		this.getResponse("/auth/logout", params);
	}
}
