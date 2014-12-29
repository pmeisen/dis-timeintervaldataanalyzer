package net.meisen.dissertation.server;

import java.util.Map;

import net.meisen.dissertation.config.xslt.DefaultValues;
import net.meisen.dissertation.exceptions.AuthException;
import net.meisen.dissertation.exceptions.PermissionException;
import net.meisen.dissertation.model.auth.IAuthManager;
import net.meisen.dissertation.model.auth.permissions.Permission;
import net.meisen.dissertation.server.sessions.Session;
import net.meisen.dissertation.server.sessions.SessionManager;
import net.meisen.general.genmisc.exceptions.registry.IExceptionRegistry;
import net.meisen.general.sbconfigurator.api.IConfiguration;
import net.meisen.general.server.http.listener.api.IServlet;
import net.meisen.general.server.http.listener.util.RequestHandlingUtilities;

import org.apache.http.HttpRequest;
import org.apache.http.HttpResponse;
import org.apache.http.HttpStatus;
import org.apache.http.entity.ContentType;
import org.apache.http.entity.StringEntity;
import org.apache.http.protocol.HttpContext;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;

import com.eclipsesource.json.JsonObject;
import com.eclipsesource.json.JsonValue;

/**
 * Base implementation of a {@code Servlet}, which ensures the checking of
 * permissions and binds the session to the thread of the servlet.
 * 
 * @author pmeisen
 * 
 */
public abstract class BaseServlet implements IServlet {
	public final static String PARAM_SESSIONID = "sessionId";
	
	private final static Logger LOG = LoggerFactory
			.getLogger(BaseServlet.class);

	protected static class HandleResult {
		public String result;
		public ContentType type;

		public HandleResult() {
			this(null, null);
		}

		public HandleResult(final String result, final ContentType type) {
			this.result = result;
			this.type = type;
		}
	}

	/**
	 * The used {@code AuthManager}.
	 */
	@Autowired
	@Qualifier(DefaultValues.AUTHMANAGER_ID)
	protected IAuthManager authManager;

	/**
	 * The used {@code SessionManager}.
	 */
	@Autowired
	@Qualifier(DefaultValues.SESSIONMANAGER_ID)
	protected SessionManager sessionManager;

	/**
	 * The used {@code ExceptionRegistry}.
	 */
	@Autowired(required = false)
	@Qualifier(IConfiguration.coreExceptionRegistryId)
	protected IExceptionRegistry exceptionRegistry;

	/**
	 * Enables the base implementation to check the
	 * {@link Permission#connectHTTP}.
	 * 
	 * @return {@code true} to enable the checking for the servlet, otherwise
	 *         {@code false}
	 */
	protected boolean doHttpPermissionCheck() {
		return true;
	}

	/**
	 * Enables the base implementation to check the
	 * {@link Permission#connectHTTP}.
	 * 
	 * @return {@code true} to enable the checking for the servlet, otherwise
	 *         {@code false}
	 */
	protected boolean needValidSession() {
		return true;
	}

	/**
	 * Checks if the current user has the permission to connect via http.
	 */
	protected void checkHttpPermission() {

		// check if the permission to use this kind of connection is available
		if (!authManager.hasPermission(Permission.connectHTTP.create())) {
			exceptionRegistry.throwRuntimeException(PermissionException.class,
					1000, Permission.connectHTTP);
		}
	}

	/**
	 * Checks the session and binds it to the current thread. The method throws
	 * an exception if the session is invalid.
	 * 
	 * @param sessionId
	 *            the identifier of the session to be checked
	 * 
	 * @return the {@code Session} instance associated to the specified
	 *         {@code sessionId}
	 */
	protected Session checkSession(final String sessionId) {
		final Session session = sessionManager.getSession(sessionId, true);
		authManager.bind(session);

		return session;
	}

	@Override
	public void handle(final HttpRequest request, final HttpResponse response,
			final HttpContext context) {

		// TODO: we should make this one configurable... otherwise attacks may
		// be possible
		response.setHeader("Access-Control-Allow-Origin", "*");

		// check if we have an options call
		if ("OPTIONS".equals(request.getRequestLine().getMethod())) {

			// reply with an empty text
			final StringEntity entity = new StringEntity("",
					ContentType.TEXT_PLAIN);
			response.setEntity(entity);
		} else {

			String result;
			ContentType type;
			try {
				final HandleResult handleRes = _handle(request, response,
						context);
				result = handleRes.result;
				type = handleRes.type;
			} catch (final AuthException e) {
				if (LOG.isDebugEnabled()) {
					LOG.error("Invalid request considering permissions.", e);
				}

				response.setStatusCode(HttpStatus.SC_FORBIDDEN);
				result = wrapExceptionToJson(e);
				type = ContentType.APPLICATION_JSON;
			} catch (final Exception e) {
				if (LOG.isErrorEnabled()) {
					LOG.error("Request failed because of a failure.", e);
				}

				response.setStatusCode(HttpStatus.SC_INTERNAL_SERVER_ERROR);
				result = wrapExceptionToJson(e);
				type = ContentType.APPLICATION_JSON;
			} finally {

				// make sure the current session is unbound from the current
				// user
				authManager.unbind();
			}

			// create the answer
			final StringEntity entity = new StringEntity(result, type);
			response.setEntity(entity);
		}
	}

	protected HandleResult _handle(final HttpRequest request,
			final HttpResponse response, final HttpContext context)
			throws Exception {

		// get the parameters
		final Map<String, String> parameters = RequestHandlingUtilities
				.parsePostParameter(request);
		if (LOG.isTraceEnabled()) {
			LOG.trace("Received parameters with request: " + parameters);
		}

		// first of all the user has to be checked in
		if (needValidSession()) {
			final Session session = checkSession(parameters.get(PARAM_SESSIONID));
			session.markAsUsed();
		}

		// do a check first
		if (doHttpPermissionCheck()) {
			checkHttpPermission();
		}

		// now handle the request
		final Object tmpResult = handleRequest(request, parameters);

		// determine the representation of the result
		String result;
		ContentType type;
		if (tmpResult == null) {
			type = ContentType.APPLICATION_JSON;
			result = JsonValue.NULL.toString();
		} else if (tmpResult instanceof JsonObject) {
			type = ContentType.APPLICATION_JSON;
			result = tmpResult.toString();
		} else if (tmpResult instanceof String) {
			type = getResponseContentType();
			result = tmpResult.toString();
		} else {
			if (LOG.isErrorEnabled()) {
				LOG.error("Unsupported type '" + tmpResult
						+ "' returned by request-handling.");
			}
			type = ContentType.APPLICATION_JSON;
			result = JsonValue.NULL.toString();
		}

		return new HandleResult(result, type);
	}

	/**
	 * Method to be implemented by the concrete implementation.
	 * 
	 * @param request
	 *            the request asked for
	 * @param parameters
	 *            the post-parameters retrieved from the request
	 * @return the result of the handling, can be {@code null}, a
	 *         {@code JsonValue}, or a string usable by the client
	 * 
	 * @throws Exception
	 *             if an exception is fired during the handling
	 */
	protected abstract Object handleRequest(final HttpRequest request,
			final Map<String, String> parameters) throws Exception;

	/**
	 * Method defining the type of the {@code Object} returned by the
	 * {@link #handleRequest(HttpRequest, Map)}.
	 * 
	 * @return the type of the result of the {@code handleRequest} method
	 */
	protected ContentType getResponseContentType() {
		return ContentType.APPLICATION_JSON;
	}

	/**
	 * Wraps the exception into a {@code Json} instance.
	 * 
	 * @param e
	 *            the exception to be wrapped
	 * 
	 * @return the JSON representing the exception
	 */
	protected String wrapExceptionToJson(final Exception e) {
		final JsonObject wrappedException = new JsonObject().add("type",
				"error").add("message", e.getLocalizedMessage());

		return wrappedException.toString();
	}
}
