package net.meisen.dissertation.server.sessions;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.InputStream;
import java.net.URI;
import java.net.URISyntaxException;
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
import net.meisen.dissertation.model.parser.query.IResourceResolver;
import net.meisen.dissertation.server.CancellationException;
import net.meisen.general.genmisc.exceptions.registry.IExceptionRegistry;
import net.meisen.general.genmisc.types.Files;
import net.meisen.general.genmisc.types.Objects;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;

/**
 * The manager to handle the different sessions of the system.
 * 
 * @author pmeisen
 * 
 */
public class SessionManager implements IResourceResolver {
	/**
	 * The name of the protocol used to resolve resources of a session.
	 */
	protected final static String RESOLVER_PROTOCOL = "uploaded";
	/**
	 * The syntax of an {@code URI} of the protocol used to resolve resources of
	 * a session.
	 */
	protected final static String RESOLVER_SYNTAX = RESOLVER_PROTOCOL
			+ "://[sessionId]/[file]";

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

	/**
	 * Default constructor which creates a manager cleaning the invalid sessions
	 * automatically.
	 */
	public SessionManager() {
		this(true);
	}

	/**
	 * Constructor used to specify if the sessions should be cleaned
	 * automatically if invalid.
	 * 
	 * @param enableAutoCleanUp
	 *            {@code true} if the sessions should be cleaned automatically,
	 *            otherwise {@code false}
	 */
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

	/**
	 * Cleans up the sessions of the manager.
	 */
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

	/**
	 * Gets a list of currently available and active sessions.
	 * 
	 * @return a collection of currently available and active sessions
	 */
	public Collection<Session> getActiveSessions() {
		return Collections.unmodifiableCollection(this.sessions.values());
	}

	/**
	 * Creates a session for the specified user-name. The method doesn't check
	 * if there is another session bound to the specified user-name.
	 * 
	 * @param username
	 *            the name of the user the session belongs to
	 * 
	 * @return the created session
	 */
	public Session createSession(final String username) {
		final Session session = new Session(username);

		sessions.put(session.getId(), session);

		return session;
	}

	/**
	 * Gets the session with the specified identifier.
	 * 
	 * @param sessionId
	 *            the identifier of the session to be retrieved
	 * 
	 * @return the session with the specified identifier
	 */
	public Session getSession(final String sessionId) {
		return getSession(sessionId, false);
	}

	/**
	 * Gets the session with the specified identifier and can throw an exception
	 * if the session to be retrieved is invalid.
	 * 
	 * @param sessionId
	 *            the identifier of the session to be retrieved
	 * @param throwException
	 *            {@code true} if an exception should be thrown if the session
	 *            is invalid, otherwise {@code false}; the latter means that
	 *            {@code null} will be returned but no exception will be thrown
	 * 
	 * @return the valid session, {@code null} if a session for the specified
	 *         identifier does not exist, or the session is invalid
	 * 
	 * @throws SessionManagerException
	 *             if an exception should be thrown, when the session is invalid
	 *             or does not exist
	 */
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

	/**
	 * Removes the specified session from the manager.
	 * 
	 * @param sessionId
	 *            the identifier of the session to be removed
	 */
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

	/**
	 * Gets the time-out specified for the invalidity of inactive sessions.
	 * 
	 * @return the time-out specified for the invalidity of inactive sessions
	 */
	public int getTimeOutInMin() {
		return timeOutInMin;
	}

	/**
	 * Sets the time-out specified for the invalidity of inactive sessions.
	 * 
	 * @param timeOutInMin
	 *            the time-out to be used
	 */
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

	/**
	 * Releases the manager an all bound resources. After this method is called,
	 * the manager has to be re-initialized prior to any further usage.
	 */
	public void release() {
		if (scheduler != null) {
			scheduler.shutdownNow();
			try {
				scheduler.awaitTermination(1, TimeUnit.SECONDS);
			} catch (final InterruptedException e) {
				// ignore it
			}
		}

		// remove the directory
		final File tmpDir = getTempDir();
		if (tmpDir != null && tmpDir.exists()) {
			Files.deleteOnExitDir(tmpDir);
		}
	}

	/**
	 * Gets the temporary directory used by the manager.
	 * 
	 * @return the temporary directory used by the manager
	 */
	public File getTempDir() {
		if (tmpDir == null) {
			exceptionRegistry.throwException(SessionManagerException.class,
					1004, tmpDir);
			return null;
		} else if (!tmpDir.exists() && !tmpDir.mkdirs()) {
			exceptionRegistry.throwException(SessionManagerException.class,
					1005, Files.getCanonicalPath(tmpDir));
			return null;
		}

		return tmpDir;
	}

	/**
	 * Sets the temporary directory to be used by the manager.
	 * 
	 * @param tmpDir
	 *            the temporary directory to be used by the manager
	 */
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

	/**
	 * Sets the temporary directory to be used by the manager.
	 * 
	 * @param tmpDir
	 *            the temporary directory to be used by the manager
	 */
	public void setTmpDir(final String tmpDir) {
		final String canPath = Files.getCanonicalPath(tmpDir);

		if (canPath == null) {
			exceptionRegistry.throwException(SessionManagerException.class,
					1004, tmpDir);
		} else {
			setTempDir(new File(canPath));
		}
	}

	/**
	 * Resets the temporary directory to the default one, i.e. a randomly
	 * generated directory within the temporary directory of the system.
	 */
	public void resetTempDir() {
		setTempDir(new File(defTmpDir));
	}

	/**
	 * Resolves the specified resource (using the syntax defined by
	 * {@link #RESOLVER_SYNTAX}).
	 * 
	 * @param resource
	 *            the identifier of the resource to be retrieved
	 * 
	 * @throws SessionManagerException
	 *             if the syntax of the resource specified was invalid, if the
	 *             resource cannot be resolved, or if the resource does not
	 *             exist
	 */
	@Override
	public InputStream resolve(final String resource)
			throws CancellationException, SessionManagerException {

		final URI uri;
		if (resource == null || resource.isEmpty()) {
			exceptionRegistry.throwException(SessionManagerException.class,
					1007, resource, RESOLVER_SYNTAX);
			return null;
		} else {

			try {
				uri = new URI(resource);
			} catch (final URISyntaxException e) {
				exceptionRegistry.throwException(SessionManagerException.class,
						1007, e, resource, RESOLVER_SYNTAX);
				return null;
			}
		}

		// validate the uri
		if (!Objects.equals(uri.getFragment(), null)
				|| !Objects.equals(uri.getPort(), -1)
				|| !Objects.equals(uri.getQuery(), null)
				|| !Objects.equals(uri.getUserInfo(), null)) {
			exceptionRegistry.throwException(SessionManagerException.class,
					1008, resource, uri, RESOLVER_SYNTAX);
		} else if (!RESOLVER_PROTOCOL.equalsIgnoreCase(uri.getScheme())) {
			exceptionRegistry.throwException(SessionManagerException.class,
					1009, resource, uri.getScheme(), RESOLVER_SYNTAX);
		} else if (!Objects.equals(uri.getAuthority(), uri.getHost())) {
			exceptionRegistry.throwException(SessionManagerException.class,
					1008, resource, uri, RESOLVER_SYNTAX);
		} else if (!uri.getPath().matches("/[^/]+")) {
			exceptionRegistry
					.throwException(SessionManagerException.class, 1010,
							resource, uri.getPath().substring(1),
							RESOLVER_SYNTAX);
		}

		// get the needed values
		final String sessionId = uri.getHost();
		final String filename = uri.getPath().substring(1);
		
		// check the session
		this.getSession(sessionId, true);
		final File file = getFile(sessionId, filename);

		try {
			return new FileInputStream(file);
		} catch (final FileNotFoundException e) {
			exceptionRegistry.throwException(SessionManagerException.class,
					1011, Files.getCanonicalPath(file));
			return null;
		}
	}

	/**
	 * Gets the specified file of the
	 * 
	 * @param sessionId
	 *            the session's identifier to look for the file
	 * @param filename
	 *            the name of the file to be read
	 * @return the file instance
	 * 
	 * @throws SessionManagerException
	 *             if the file does not exist, cannot be read, is not a file, or
	 *             if the session's directory is unavailble
	 */
	public File getFile(final String sessionId, final String filename)
			throws SessionManagerException {

		final File dir = this.getSessionDir(sessionId, false);
		if (dir == null) {
			exceptionRegistry.throwException(SessionManagerException.class,
					1012, filename, sessionId);
		}
		final File file = new File(dir, filename);
		if (file == null || !file.exists() || !file.isFile() || !file.canRead()) {
			exceptionRegistry.throwException(SessionManagerException.class,
					1011, Files.getCanonicalPath(file));
		}
		return file;
	}

	/**
	 * Sets the {@code ExceptionRegistry} for the manager. The registry is also
	 * auto-wired.
	 * 
	 * @param exceptionRegistry
	 *            the {@code ExceptionRegistry} for the manager, cannot be
	 *            {@code null}
	 * 
	 * @see IExceptionRegistry
	 */
	public void setExceptionRegistry(final IExceptionRegistry exceptionRegistry) {
		this.exceptionRegistry = exceptionRegistry;
	}
}
