package net.meisen.dissertation.server.sessions;

import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class SessionManager {
	private final static Logger LOG = LoggerFactory
			.getLogger(SessionManager.class);

	private Map<String, Session> sessions;

	private int timeOutInMin;

	public SessionManager() {
		sessions = new ConcurrentHashMap<String, Session>();
		timeOutInMin = 30;
	}

	public Session createSession(final String username) {
		final Session session = new Session(username);

		sessions.put(session.getId(), session);

		return session;
	}

	public Session getSession(final String sessionId) {
		return getSession(sessionId, false);
	}

	public Session getSession(final String sessionId,
			final boolean throwException) {
		final Session session = sessions.get(sessionId);

		if (session == null) {
			if (throwException) {
				// TODO throw exception showing invalid sessionId
				throw new IllegalStateException("Invalid session");
			} else {
				return null;
			}
		} else if (session.isTimedOut(timeOutInMin)) {
			sessions.remove(session);

			if (throwException) {
				// TODO throw exception showing time-out
				throw new IllegalStateException("Session expired");
			} else {
				return null;
			}
		} else {
			return session;
		}
	}

	public void removeSession(final String sessionId) {
		Session session;

		if ((session = sessions.remove(sessionId)) != null) {
			if (LOG.isDebugEnabled()) {
				LOG.debug("Removed session '" + sessionId + "' of user '"
						+ session.getUsername() + "'.");
			}
		}
	}

	public int getTimeOutInMin() {
		return timeOutInMin;
	}

	public void setTimeOutInMin(final int timeOutInMin) {
		if (timeOutInMin <= 0) {
			if (LOG.isWarnEnabled()) {
				LOG.warn("Tried to set an invalid time-out value of '"
						+ timeOutInMin + "'.");
			}
		} else {
			this.timeOutInMin = timeOutInMin;
		}
	}
}
