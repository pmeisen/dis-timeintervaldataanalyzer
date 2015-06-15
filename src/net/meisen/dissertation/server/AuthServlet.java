package net.meisen.dissertation.server;

import java.util.Map;
import java.util.Set;

import net.meisen.dissertation.model.auth.permissions.DefinedPermission;
import net.meisen.dissertation.server.sessions.Session;
import net.meisen.general.genmisc.types.Dates;
import net.meisen.general.server.http.listener.util.RequestHandlingUtilities;
import net.meisen.general.server.settings.pojos.Extension;

import org.apache.http.HttpRequest;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.eclipsesource.json.JsonArray;
import com.eclipsesource.json.JsonObject;

/**
 * The {@code AuthServlet} is used to authenticate against the running server.
 * 
 * @author pmeisen
 * 
 */
public class AuthServlet extends BaseServlet {
	private final static Logger LOG = LoggerFactory
			.getLogger(AuthServlet.class);

	@Override
	public void initialize(final Extension e) {
		// nothing to do
	}

	@Override
	protected boolean doHttpPermissionCheck() {
		return false;
	}

	@Override
	protected boolean needValidSession() {
		return false;
	}

	@Override
	protected JsonObject handleRequest(final HttpRequest request,
			final Map<String, String> parameters) {
		final String path = RequestHandlingUtilities.getPath(request);
		final int pos = path.lastIndexOf("/");
		final String method = pos == -1 ? "" : path.substring(pos + 1);

		if ("login".equals(method)) {
			final String username;
			final String password;
			if (parameters.containsKey("credentials")) {
				final JsonObject credentials = JsonObject.readFrom(parameters
						.get("credentials"));

				// get username and password from the credentials
				username = credentials.get("username").toString();
				password = credentials.get("password").toString();
			} else if (parameters.containsKey("username")
					&& parameters.containsKey("password")) {

				username = parameters.get("username");
				password = parameters.get("password");
			} else if (parameters.containsKey("sessionId")) {
				final Session session = this.checkSession(parameters
						.get("sessionId"));
				username = session.getUsername();
				password = null;
			} else {
				username = null;
				password = null;
			}

			// use the authManager to authenticate
			authManager.login(username, password);

			// check if the permission to use this connection is available
			checkHttpPermission();

			// now assign the user a session and check it
			final Session session = sessionManager.createSession(username);
			checkSession(session.getId());

			// keep the valid login
			if (LOG.isDebugEnabled()) {
				LOG.debug("Connected user '" + username + "'.");
			}

			final Set<DefinedPermission> permissions = authManager
					.getUserPermissions(session.getUsername());
			final JsonArray perms = new JsonArray();
			for (final DefinedPermission permission : permissions) {
				perms.add(permission.toString());
			}

			// send the sessionsId
			return new JsonObject()
					.add("sessionId", session.getId())
					.add("username", session.getUsername())
					.add("logintime",
							Dates.createStringFromDate(
									session.getCreationDate(),
									"dd.MM.yyyy HH:mm:ss"))
					.add("lasttime",
							Dates.createStringFromDate(
									session.getLastAccessTime(),
									"dd.MM.yyyy HH:mm:ss"))
					.add("permissions", perms)
					.add("timeOutInMin", sessionManager.getTimeOutInMin());
		} else if ("logout".equals(method)) {
			final String sessionId = parameters.get("sessionId");
			sessionManager.removeSession(sessionId);
			authManager.logout();

			return new JsonObject().add("sessionId", (String) null);
		} else if ("ping".equals(method)) {
			final String sessionId = parameters.get("sessionId");

			// check the session and the permission
			final Session session = checkSession(sessionId);
			checkHttpPermission();

			// refresh the session
			session.markAsUsed();

			return new JsonObject().add("sessionId", sessionId);
		} else if ("userinfo".equals(method)) {
			final String sessionId = parameters.get("sessionId");
			final Session session = checkSession(sessionId);

			final int timeoutInMin = sessionManager.getTimeOutInMin();
			return new JsonObject()
					.add("sessionId", session.getId())
					.add("username", session.getUsername())
					.add("logintime",
							Dates.createStringFromDate(
									session.getCreationDate(),
									"dd.MM.yyyy HH:mm:ss"))
					.add("leftTimeoutInMin",
							session.getLeftTimeoutInMin(timeoutInMin))
					.add("timeoutInMin", timeoutInMin);
		} else {
			// TODO throw exception
			throw new IllegalStateException("Unsupported method called.");
		}
	}

	@Override
	protected boolean measurePerformace() {
		return false;
	}
}
