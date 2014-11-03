package net.meisen.dissertation.server;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

import java.io.UnsupportedEncodingException;
import java.util.HashMap;
import java.util.Map;

import net.meisen.dissertation.model.auth.permissions.Permission;

import org.junit.Test;

import com.eclipsesource.json.JsonArray;
import com.eclipsesource.json.JsonObject;
import com.eclipsesource.json.JsonValue;

/**
 * Tests the implementation of the {@code QueryServlet}.
 * 
 * @author pmeisen
 * 
 */
public class TestQueryServlet extends BaseTestWithServerConnection {

	@Override
	public String getConfig() {
		return "net/meisen/dissertation/server/testShiroAuthConfig.xml";
	}

	@Override
	public boolean isHttp() {
		return true;
	}

	/**
	 * Tests the answer of the {@code QueryServlet} if a single query is fired.
	 * 
	 * @throws UnsupportedEncodingException
	 *             if the encoding is unsupported
	 */
	@Test
	public void testSingleStringQuery() throws UnsupportedEncodingException {
		signIn();

		final Map<String, String> params = new HashMap<String, String>();
		params.put("query", "GET USERS");

		// handle the response
		final byte[] response = this.getResponse("/query/tsql", params);
		final JsonValue result = JsonValue
				.readFrom(new String(response, "UTF8"));
		assertTrue(result.isObject());
		System.out.println(result);
		assertEquals("set", ((JsonObject) result).get("type").asString());
	}

	/**
	 * Tests the answer of the {@code QueryServlet} if a single tsql query is
	 * fired.
	 * 
	 * @throws UnsupportedEncodingException
	 *             if the encoding is unsupported
	 */
	@Test
	public void testSingleJsonQuery() throws UnsupportedEncodingException {
		signIn();

		final Map<String, String> params = new HashMap<String, String>();
		final JsonValue value = JsonValue.valueOf("GET USERS");
		params.put("query", value.toString());

		// handle the response
		final byte[] response = this.getResponse("/query/tsql", params);
		final JsonValue result = JsonValue
				.readFrom(new String(response, "UTF8"));
		assertTrue(result.isObject());
		assertEquals("set", ((JsonObject) result).get("type").asString());
	}

	/**
	 * Tests the answer of the {@code QueryServlet} if multiple tsql queries are
	 * fired.
	 * 
	 * @throws UnsupportedEncodingException
	 *             if the encoding is unsupported
	 */
	@Test
	public void testMultipleQueries() throws UnsupportedEncodingException {
		signIn();

		final Map<String, String> params = new HashMap<String, String>();
		final JsonArray array = new JsonArray().add("GET MODELS").add(
				"GET PERMISSIONS");
		params.put("queries", array.toString());

		// handle the response
		final byte[] response = this.getResponse("/query/tsql", params);
		final JsonValue result = JsonValue
				.readFrom(new String(response, "UTF8"));
		assertTrue(result.isArray());

		// check the result
		JsonObject jsonObj;
		assertEquals(2, ((JsonArray) result).size());
		assertTrue(((JsonArray) result).get(0).isObject());
		jsonObj = (JsonObject) ((JsonArray) result).get(0);
		assertEquals("set", jsonObj.get("type").asString());
		assertTrue(((JsonArray) result).get(1).isObject());
		jsonObj = (JsonObject) ((JsonArray) result).get(1);
		assertEquals("set", jsonObj.get("type").asString());
	}

	/**
	 * Tests the implementation of the {@code QueryServlet} to retrieve the
	 * permissions of the system.
	 * 
	 * @throws UnsupportedEncodingException
	 *             if the encoding is unsupported
	 */
	@Test
	public void testSystemPermissionQuery() throws UnsupportedEncodingException {
		signIn();

		final Map<String, String> params = new HashMap<String, String>();
		params.put("object", "permissions");

		// handle the response
		final byte[] response = this.getResponse("/query/system", params);
		final JsonValue result = JsonValue
				.readFrom(new String(response, "UTF8"));
		assertTrue(result.isArray());
		assertEquals(Permission.values().length, ((JsonArray) result).size());
	}

	/**
	 * Tests the implementation of the {@code QueryServlet} to retrieve user
	 * information of a {@code null} user.
	 * 
	 * @throws UnsupportedEncodingException
	 *             if the encoding is unsupported
	 */
	@Test
	public void testSystemUserNullQuery() throws UnsupportedEncodingException {
		signIn();

		final Map<String, String> params = new HashMap<String, String>();
		params.put("object", "user");
		params.put("username", null);

		// handle the response
		final byte[] response = this.getResponse("/query/system", params);
		final JsonValue result = JsonValue
				.readFrom(new String(response, "UTF8"));
		assertTrue(result.isObject());
		assertTrue(((JsonObject) result).get("username").isNull());
		assertTrue(((JsonObject) result).get("permissions").isArray());
		assertEquals(
				((JsonArray) ((JsonObject) result).get("permissions")).size(),
				0);
		assertTrue(((JsonObject) result).get("roles").isArray());
		assertEquals(((JsonArray) ((JsonObject) result).get("roles")).size(), 0);
	}

	/**
	 * Tests the implementation of the {@code QueryServlet} to retrieve user
	 * information of the admin user.
	 * 
	 * @throws UnsupportedEncodingException
	 *             if the encoding is unsupported
	 */
	@Test
	public void testSystemUserAdminQuery() throws UnsupportedEncodingException {
		signIn();

		final Map<String, String> params = new HashMap<String, String>();
		params.put("object", "user");
		params.put("username", "admin");

		// handle the response
		final byte[] response = this.getResponse("/query/system", params);
		final JsonValue result = JsonValue
				.readFrom(new String(response, "UTF8"));

		assertTrue(result.isObject());
		assertEquals("admin", ((JsonObject) result).get("username").asString());
		assertTrue(((JsonObject) result).get("permissions").isArray());
		assertEquals(
				((JsonArray) ((JsonObject) result).get("permissions")).size(),
				Permission.values().length);
		assertTrue(((JsonObject) result).get("roles").isArray());
		assertEquals(((JsonArray) ((JsonObject) result).get("roles")).size(), 0);
	}
}
