package net.meisen.dissertation.server;

import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.fail;
import static org.junit.Assert.assertTrue;

import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.io.UnsupportedEncodingException;
import java.sql.DriverManager;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Properties;
import java.util.UUID;

import net.meisen.dissertation.help.ExceptionBasedTest;
import net.meisen.dissertation.jdbc.TidaConnection;
import net.meisen.general.genmisc.types.Files;
import net.meisen.general.genmisc.types.Streams;

import org.apache.http.HttpEntity;
import org.apache.http.HttpResponse;
import org.apache.http.NameValuePair;
import org.apache.http.client.entity.UrlEncodedFormEntity;
import org.apache.http.client.methods.HttpEntityEnclosingRequestBase;
import org.apache.http.client.methods.HttpGet;
import org.apache.http.client.methods.HttpPost;
import org.apache.http.client.methods.HttpRequestBase;
import org.apache.http.impl.client.CloseableHttpClient;
import org.apache.http.impl.client.HttpClientBuilder;
import org.apache.http.message.BasicNameValuePair;
import org.apache.http.util.EntityUtils;
import org.junit.After;
import org.junit.Before;

import com.eclipsesource.json.JsonObject;

/**
 * An implementation used for tests with a server connection.
 * 
 * @author pmeisen
 * 
 */
public abstract class BaseTestWithServerConnection extends ExceptionBasedTest {

	/**
	 * The connection to the server
	 */
	protected TidaConnection conn;

	/**
	 * The started server instance.
	 */
	protected TidaServer server;

	/**
	 * Is used by the implementation to keep the current sessionId. Use
	 * {@link #signIn()} to sign-in and set the parameter.
	 */
	protected String sessionId = null;

	/**
	 * Start a test-server for testing purposes.
	 * 
	 * @throws SQLException
	 *             if the connection could not be created
	 */
	@Before
	public void startServer() throws SQLException {
		final Properties properties = new Properties();

		if (createTemporaryFolder()) {
			Files.deleteOnExitDir(
					new File(System.getProperty("java.io.tmpdir")),
					getTestLocation() + "-.*");

			// create the directory
			assertTrue(getTemporaryFolder().exists()
					|| getTemporaryFolder().mkdirs());
		}

		if (getConfig() != null) {
			properties.setProperty("tida.config.selector", getConfig());

			if (createTemporaryFolder()) {
				properties.setProperty("tida.config.test.location",
						Files.getCanonicalPath(getTemporaryFolder()));
			} else {
				properties.setProperty("tida.config.test.location",
						"!please activate temporary folder creation!");
			}
		} else if (createTemporaryFolder()) {
			properties.setProperty("tida.model.test.location",
					Files.getCanonicalPath(getTemporaryFolder()));
		}
		properties.setProperty("tida.server.tsql.port", "" + getTsqlPort());
		properties.setProperty("tida.server.http.port", "" + getHttpPort());
		properties.setProperty("tida.server.tsql.enabled", "" + isTSQL());
		properties.setProperty("tida.server.http.enabled", "" + isHttp());

		server = TidaServer.create(properties);
		server.startAsync();

		conn = (TidaConnection) DriverManager.getConnection(getJdbc());
	}

	/**
	 * Defines if the server should enable {@code TSQL} when starting.
	 * 
	 * @return {@code true} if {@code TSQL} should be enabled, otherwise
	 *         {@code false}
	 */
	public boolean isTSQL() {
		return false;
	}

	/**
	 * Defines if the server should enable {@code HTTP} when starting.
	 * 
	 * @return {@code true} if {@code HTTP} should be enabled, otherwise
	 *         {@code false}
	 */
	public boolean isHttp() {
		return false;
	}

	/**
	 * CleanUp after the test.
	 * 
	 * @throws SQLException
	 *             if the connection could not be closed
	 */
	@After
	public void shutdownServer() throws SQLException {

		// sign-off
		signOff();

		if (this.conn != null) {
			this.conn.close();
		}

		if (server != null) {
			server.shutdown(true);
		}

		if (createTemporaryFolder()) {
			Files.deleteOnExitDir(
					new File(System.getProperty("java.io.tmpdir")),
					getTestLocation() + "-.*");
		}
	}

	/**
	 * Defines if a temporary directory should be created for the test.
	 * 
	 * @return {@code true} if a temporary directory should be created,
	 *         otherwise {@code false}
	 */
	public boolean createTemporaryFolder() {
		return true;
	}

	/**
	 * Gets the location of the temporary folder created for the test. The
	 * location is also available using the property
	 * {@code tida.config.test.location}.
	 * 
	 * @return the location of the temporary folder created for the test
	 */
	public File getTemporaryFolder() {
		return new File(System.getProperty("java.io.tmpdir"), getTestLocation()
				+ "-" + UUID.randomUUID().toString());
	}

	/**
	 * A folder which can be used by a test to store configuration information,
	 * e.g. modify the default location of the model-data.
	 * 
	 * @return folder to be used for the test
	 */
	public String getTestLocation() {
		return getClass().getSimpleName();
	}

	/**
	 * Gets the used {@code TSQL} port of the server.
	 * 
	 * @return the used {@code TSQL} port of the server
	 */
	public int getTsqlPort() {
		return 6666;
	}

	/**
	 * Gets the used {@code HTTP} port of the server.
	 * 
	 * @return the used {@code HTTP} port of the server
	 */
	public int getHttpPort() {
		return 6667;
	}

	/**
	 * Gets the selector for the configuration of the test.
	 * 
	 * @return the configuration used for the test
	 */
	public String getConfig() {
		return null;
	}

	/**
	 * Gets the JDBC-URL to connect to the server.
	 * 
	 * @return the JDBC-URL to connect to the server
	 */
	public String getJdbc() {
		return getJdbc("admin");
	}

	/**
	 * Method to create a JDBC for a specific user.
	 * 
	 * @param username
	 *            the name of the user to create the JDBC for
	 * 
	 * @return the created JDBC-URL
	 */
	public String getJdbc(final String username) {
		return "jdbc:tida://" + username + ":password@localhost:"
				+ getTsqlPort();
	}

	/**
	 * Helper method to get the response requesting the specified {@code suffix}
	 * .
	 * 
	 * @param suffix
	 *            the suffix of the base-url to request
	 * 
	 * @return the answer
	 */
	public byte[] getResponse(final String suffix) {
		return getResponse(suffix, "get", null);
	}

	/**
	 * Helper method to get the response requesting the specified {@code suffix}
	 * and the specified {@code entity}.
	 * 
	 * @param suffix
	 *            the suffix of the base-url to request
	 * @param params
	 *            the parameters send with the request
	 * 
	 * @return the answer
	 */
	public byte[] getResponse(final String suffix,
			final List<NameValuePair> params) {
		return getResponse(suffix, "post", params);
	}

	/**
	 * Helper method to get the response requesting the specified {@code suffix}
	 * and the specified {@code entity}.
	 * 
	 * @param suffix
	 *            the suffix of the base-url to request
	 * @param params
	 *            the parameters send with the request
	 * 
	 * @return the answer
	 */
	public byte[] getResponse(final String suffix,
			final Map<String, String> params) {
		final List<NameValuePair> list = new ArrayList<NameValuePair>();
		for (final Entry<String, String> entry : params.entrySet()) {
			list.add(new BasicNameValuePair(entry.getKey(), entry.getValue()));
		}

		return getResponse(suffix, "post", list);
	}

	/**
	 * Helper method to get the response requesting the specified {@code suffix}
	 * .
	 * 
	 * @param suffix
	 *            the suffix of the base-url to request
	 * @param method
	 *            the method used to send the data, i.e. get or post
	 * @param params
	 *            the parameters send with the request (only available if post
	 *            is used as method)
	 * 
	 * @return the answer
	 */
	public byte[] getResponse(final String suffix, String method,
			List<NameValuePair> params) {

		// check if we have a session
		if (sessionId != null) {
			if (params == null) {
				params = new ArrayList<NameValuePair>();
			}

			params.add(new BasicNameValuePair("sessionId", sessionId));
			method = "post";
		}

		final HttpClientBuilder httpClientBuilder = HttpClientBuilder.create();
		final CloseableHttpClient httpClient = httpClientBuilder.build();
		final String uri = "http://localhost:" + getHttpPort() + "/" + suffix;
		final HttpRequestBase httpRequest = method.equalsIgnoreCase("post") ? new HttpPost(
				uri) : new HttpGet(uri);

		if (httpRequest instanceof HttpEntityEnclosingRequestBase) {
			try {
				((HttpEntityEnclosingRequestBase) httpRequest)
						.setEntity(new UrlEncodedFormEntity(params, "UTF-8"));
			} catch (final UnsupportedEncodingException e) {
				// ignore silently
			}
		}

		try {
			final HttpResponse response = httpClient.execute(httpRequest);
			return getResponse(response);
		} catch (final RuntimeException e) {
			// In case of an unexpected exception you may want to abort
			// the HTTP request in order to shut down the underlying
			// connection immediately.
			httpRequest.abort();
			fail(e.getMessage());
		} catch (final Exception e) {
			fail(e.getMessage());
		}

		return null;
	}

	/**
	 * Get the response as byte-array.
	 * 
	 * @param response
	 *            the response
	 * 
	 * @return the byte array of the response
	 */
	public byte[] getResponse(final HttpResponse response) {
		HttpEntity entity = null;
		InputStream instream = null;
		try {
			entity = response.getEntity();

			// the only right way out of here
			if (entity != null) {
				instream = entity.getContent();
				return Streams.copyStreamToByteArray(instream);
			}
		} catch (final Exception e) {
			fail(e.getMessage());
		} finally {
			Streams.closeIO(instream);
			if (entity != null) {
				try {
					EntityUtils.consume(entity);
				} catch (final IOException e) {
					// ignore
				}
			}
		}

		return null;
	}

	/**
	 * Method to sign-in. The implementation keeps track and signs-off if
	 * needed.
	 * 
	 * @return the sessionId of the session
	 */
	public String signIn() {
		final Map<String, String> params = new HashMap<String, String>();
		params.put("username", "admin");
		params.put("password", "password");

		// make sure we got a sessionId
		JsonObject value;
		try {
			value = JsonObject.readFrom(new String(getResponse("/auth/login",
					params), "UTF8"));
		} catch (final UnsupportedEncodingException e) {
			fail("Cannot happen '" + e.getMessage() + "'.");
			value = null;
		}
		assertNotNull(value);

		// let's try to login again, should lead to an exception
		this.sessionId = value.get("sessionId").asString();
		assertNotNull(this.sessionId);

		return this.sessionId;
	}

	/**
	 * Method to sign-off.
	 */
	public void signOff() {

		if (sessionId != null) {

			// make sure we got a sessionId
			getResponse("/auth/logout");
			sessionId = null;
		}
	}
}
