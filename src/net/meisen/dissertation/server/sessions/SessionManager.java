package net.meisen.dissertation.server.sessions;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.UUID;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.Executors;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.TimeUnit;

import net.meisen.dissertation.config.xslt.DefaultValues;
import net.meisen.dissertation.exceptions.SessionManagerException;
import net.meisen.general.genmisc.exceptions.registry.IExceptionRegistry;
import net.meisen.general.genmisc.types.Files;

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
	@Qualifier(DefaultValues.EXCEPTIONREGISTRY_ID)
	private IExceptionRegistry exceptionRegistry;

	private Map<String, Session> sessions;

	private String defTmpDir = Files.getCanonicalPath(new File(System
			.getProperty("java.io.tmpdir"), getClass().getSimpleName() + "-"
			+ UUID.randomUUID().toString()));
	private File tmpDir;

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

			tmpDir = new File(defTmpDir);
		} else {
			scheduler = null;
			tmpDir = null;
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

		// remove the directory
		final File tmpDir = getTempDir();
		if (tmpDir != null && tmpDir.exists()) {
			Files.deleteOnExitDir(tmpDir);
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
			removeSession(sessionId);

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
			return;
		}

		final File sessionDir = getSessionDir(sessionId, false);
		if ((session = sessions.remove(sessionId)) != null) {

			// delete the directory
			final File tmpDir = getTempDir();
			if (tmpDir != null) {

				// remove if it exists
				if (sessionDir.exists() && sessionDir.isDirectory()) {

					if (Files.deleteDir(sessionDir)) {
						if (LOG.isDebugEnabled()) {
							LOG.debug("Removed session-directory '"
									+ sessionDir + "'.");
						}
					} else {
						if (LOG.isWarnEnabled()) {
							LOG.warn("Unable to remove the session's directory '"
									+ sessionDir + "'.");
						}
					}
				}
			}

			if (LOG.isDebugEnabled()) {
				LOG.debug("Removed session '" + sessionId + "' of user '"
						+ session.getUsername() + "'.");
			}
		}
	}

	/**
	 * Gets the directory used to store values of the specified {@code Session}.
	 * 
	 * @param sessionId
	 *            the identifier of the session to determine the folder for
	 * @param create
	 *            {@code true} if the directory should be created, if it does
	 *            not exists
	 * 
	 * @return the session's directory or {@code null} if the session does not
	 *         exists, or if the {@code tmpDir} was {@code null}
	 */
	public File getSessionDir(final String sessionId, final boolean create) {
		final File tmpDir = getTempDir();

		if (tmpDir == null) {
			return null;
		} else if (!sessions.containsKey(sessionId)) {
			return null;
		}

		// get the directory
		final File sessionDir = new File(tmpDir, sessionId);

		// create the directory if needed
		if (create) {

			// check if the directory exists
			if (!sessionDir.exists()) {

				// make the directories
				if (!sessionDir.mkdirs()) {
					exceptionRegistry.throwException(
							SessionManagerException.class, 1003, sessionDir);
					return null;
				}
			}
		}

		return sessionDir;
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

	public File getTempDir() {
		if (tmpDir != null && !tmpDir.exists() && !tmpDir.mkdirs()) {
			exceptionRegistry.throwException(SessionManagerException.class,
					1005, Files.getCanonicalPath(tmpDir));
			return null;
		}

		return tmpDir;
	}

	public void setTempDir(final File tmpDir) {
		if (this.tmpDir == null) {
			// nothing to do
		} else if (this.tmpDir.equals(tmpDir)) {
			return;
		} else {

			// warn if the directory is changed while active sessions are there
			if (LOG.isWarnEnabled() && sessions.size() > 0) {
				LOG.warn("Changing the temporary directory while sessions are active might raise unwanted side effects.");
			}

			// remove the old directory
			Files.deleteOnExitDir(this.tmpDir);
		}

		// validate some pre-requirements
		if (tmpDir == null) {
			exceptionRegistry.throwException(SessionManagerException.class,
					1004, tmpDir);
		} else if (tmpDir.exists() && tmpDir.isFile()) {
			exceptionRegistry.throwException(SessionManagerException.class,
					1006, Files.getCanonicalPath(tmpDir));
		} else if (tmpDir.exists()) {
			if (LOG.isWarnEnabled()) {
				LOG.warn("The temporary directory '"
						+ Files.getCanonicalPath(tmpDir) + "' already exists.");
			}
		} else if (!tmpDir.mkdirs()) {
			exceptionRegistry.throwException(SessionManagerException.class,
					1005, Files.getCanonicalPath(tmpDir));
		}

		if (!tmpDir.exists() || !tmpDir.isDirectory() || !tmpDir.canWrite()) {
			exceptionRegistry.throwException(SessionManagerException.class,
					1004, Files.getCanonicalPath(tmpDir));
		} else {
			this.tmpDir = tmpDir;
		}
	}

	public void setTmpDir(final String tmpdir) {
		final String canPath = Files.getCanonicalPath(tmpdir);

		if (canPath == null) {
			exceptionRegistry.throwException(SessionManagerException.class,
					1004, tmpdir);
		} else {
			setTempDir(new File(canPath));
		}
	}

	public void resetTempDir() {
		setTempDir(new File(defTmpDir));
	}
}
