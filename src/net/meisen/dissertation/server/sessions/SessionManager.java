package net.meisen.dissertation.server.sessions;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.Executors;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.TimeUnit;

import net.meisen.dissertation.exceptions.SessionManagerException;
import net.meisen.general.genmisc.exceptions.registry.IExceptionRegistry;
import net.meisen.general.sbconfigurator.api.IConfiguration;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;

public class SessionManager {
	private final static Logger LOG = LoggerFactory
			.getLogger(SessionManager.class);
	private final ScheduledExecutorService scheduler;
	private int timeOutInMin;

	@Autowired
	@Qualifier(IConfiguration.coreExceptionRegistryId)
	private IExceptionRegistry exceptionRegistry;

	private Map<String, Session> sessions;

	public SessionManager() {
		this(true);
	}

	public SessionManager(final boolean enableAutoCleanUp) {
		sessions = new ConcurrentHashMap<String, Session>();
		timeOutInMin = 30;

		if (enableAutoCleanUp) {
			scheduler = Executors.newScheduledThreadPool(1);
			scheduler.scheduleAtFixedRate(new Runnable() {

				@Override
				public void run() {
					SessionManager.this.cleanUp();
				}
			}, 1, 1, TimeUnit.MINUTES);
		} else {
			scheduler = null;
		}
	}

	public void cleanUp() {
		final int timeout = this.timeOutInMin;

		// get all the expired sessions
		final Collection<Session> sessions = getActiveSessions();
		final List<String> expiredIds = new ArrayList<String>();
		for (final Session session : sessions) {
			if (Thread.interrupted()) {
				return;
			} else if (session.isTimedOut(timeout)) {
				expiredIds.add(session.getId());
			}
		}

		// remove those that are expired
		for (final String expiredId : expiredIds) {
			removeSession(expiredId);
		}
	}

	public Collection<Session> getActiveSessions() {
		return Collections.unmodifiableCollection(this.sessions.values());
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
			final boolean throwException) throws SessionManagerException {
		if (sessionId == null) {
			if (throwException) {
				exceptionRegistry.throwRuntimeException(
						SessionManagerException.class, 1000);
			}
			return null;
		}

		// try to handle the session
		final Session session = sessions.get(sessionId);
		if (session == null) {
			if (throwException) {
				exceptionRegistry.throwRuntimeException(
						SessionManagerException.class, 1001, sessionId);
			}
			return null;
		} else if (session.isTimedOut(timeOutInMin)) {
			sessions.remove(session);

			if (throwException) {
				exceptionRegistry.throwRuntimeException(
						SessionManagerException.class, 1002, sessionId);
			}
			return null;
		} else {
			return session;
		}
	}

	public void removeSession(final String sessionId) {
		Session session;

		if (sessionId == null) {
			// nothing to do
		} else if ((session = sessions.remove(sessionId)) != null) {
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

	public void release() {
		if (scheduler != null) {
			scheduler.shutdownNow();
			try {
				scheduler.awaitTermination(1, TimeUnit.SECONDS);
			} catch (final InterruptedException e) {
				// ignore it
			}
		}
	}
}
