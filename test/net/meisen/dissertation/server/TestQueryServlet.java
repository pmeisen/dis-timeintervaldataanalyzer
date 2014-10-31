package net.meisen.dissertation.server;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

import java.io.UnsupportedEncodingException;
import java.util.HashMap;
import java.util.Map;

import org.junit.Test;

import com.eclipsesource.json.JsonArray;
import com.eclipsesource.json.JsonObject;
import com.eclipsesource.json.JsonValue;

public class TestQueryServlet extends BaseTestWithServerConnection {

	@Override
	public String getConfig() {
		return "net/meisen/dissertation/server/testShiroAuthConfig.xml";
	}

	@Override
	public boolean isHttp() {
		return true;
	}

	@Test
	public void testSingleStringQuery() throws UnsupportedEncodingException {
		signIn();

		final Map<String, String> params = new HashMap<String, String>();
		params.put("query", "GET USERS");

		// handle the response
		final byte[] response = this.getResponse("/query/run", params);
		final JsonValue result = JsonValue
				.readFrom(new String(response, "UTF8"));
		assertTrue(result.isObject());
		System.out.println(result);
		assertEquals("set", ((JsonObject) result).get("type").asString());
	}

	@Test
	public void testSingleJsonQuery() throws UnsupportedEncodingException {
		signIn();

		final Map<String, String> params = new HashMap<String, String>();
		final JsonValue value = JsonValue.valueOf("GET USERS");
		params.put("query", value.toString());

		// handle the response
		final byte[] response = this.getResponse("/query/run", params);
		final JsonValue result = JsonValue
				.readFrom(new String(response, "UTF8"));
		assertTrue(result.isObject());
		assertEquals("set", ((JsonObject) result).get("type").asString());
	}

	@Test
	public void testMultipleQueries() throws UnsupportedEncodingException {
		signIn();

		final Map<String, String> params = new HashMap<String, String>();
		final JsonArray array = new JsonArray().add("GET MODELS").add(
				"GET PERMISSIONS");
		params.put("queries", array.toString());

		// handle the response
		final byte[] response = this.getResponse("/query/run", params);
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
}
