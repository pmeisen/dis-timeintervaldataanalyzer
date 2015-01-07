package net.meisen.dissertation.server.sessions;

import java.util.Date;
import java.util.HashMap;
import java.util.Map;
import java.util.UUID;

import net.meisen.general.genmisc.types.Dates;

/**
 * Class representing a session of a user connected via {@code HTTP}.
 * 
 * @author pmeisen
 * 
 */
public class Session {

	/**
	 * Interface which can be used to override the now-Date.
	 * 
	 * @author pmeisen
	 * 
	 */
	public interface IDateProvider {

		/**
		 * Gets the current date.
		 * 
		 * @return the current date
		 */
		public Date now();
	}

	private final IDateProvider provider;
	private final String id;
	private final Date creationDate;
	private final String username;
	private final Map<String, Object> boundObjects;

	private Date lastAccessDate;

	/**
	 * Default constructor which specifies the session's username. The
	 * constructor will use the {@code new Date()} as implementation for the
	 * {@code DateProvider}.
	 * 
	 * @param username
	 *            the name of the user the session belongs to
	 * 
	 * @see IDateProvider
	 */
	public Session(final String username) {
		this(username, new IDateProvider() {

			@Override
			public Date now() {
				return new Date();
			}
		});
	}

	/**
	 * Constructor which specifies the session's username.
	 * 
	 * @param username
	 *            the name of the user the session belongs to
	 * @param provider
	 *            the {@code DateProvider} providing the current date
	 * 
	 * @see IDateProvider
	 */
	public Session(final String username, final IDateProvider provider) {
		this.boundObjects = new HashMap<String, Object>();
		this.id = UUID.randomUUID().toString();
		this.provider = provider;
		this.creationDate = provider.now();
		this.username = username;

		this.lastAccessDate = this.creationDate;
	}

	/**
	 * Binds the specified {@code object} to the current session, associated to
	 * the specified {@code identifier}.
	 * 
	 * @param identifier
	 *            the identifier used to refer to the object
	 * @param object
	 *            the object to be bound to the session
	 */
	public void bind(final String identifier, final Object object) {
		boundObjects.put(identifier, object);
	}

	/**
	 * Gets the object bound to the specified {@code identifier} within the
	 * session.
	 * 
	 * @param identifier
	 *            the identifier of the object to be retrieved
	 * 
	 * @return the object bound or {@code null} if not object was found
	 */
	@SuppressWarnings("unchecked")
	public <T> T get(final String identifier) {
		return (T) boundObjects.get(identifier);
	}

	/**
	 * Removes, i.e. unbinds, the object associated to the specified
	 * {@code identifier} from the session.
	 * 
	 * @param identifier
	 *            the identifier of the object to be unbound
	 * 
	 * @return the object that was bound or {@code null} if no object was bound
	 */
	public Object unbind(final String identifier) {
		return boundObjects.remove(identifier);
	}

	/**
	 * Gets the id of the session.
	 * 
	 * @return the id of the session
	 */
	public String getId() {
		return id;
	}

	/**
	 * Gets the name of the user of the session.
	 * 
	 * @return the name of the user of the session
	 */
	public String getUsername() {
		return username;
	}

	/**
	 * Marks the session as used. The method does not check if the session is
	 * timed-out already.
	 */
	public void markAsUsed() {
		this.lastAccessDate = new Date();
	}

	/**
	 * Checks if the session is timed out, i.e. returns {@code true} if the
	 * session is timed out, otherwise {@code false}.
	 * 
	 * @param timeoutInMin
	 *            the time-out interval
	 * 
	 * @return {@code true} if the session is timed out, otherwise {@code false}
	 */
	public boolean isTimedOut(int timeoutInMin) {
		if (timeoutInMin < 0) {
			timeoutInMin = Math.abs(timeoutInMin);
		} else if (timeoutInMin == 0) {
			return true;
		}

		final Date now = provider.now();

		// check if the date is in the past according to the creation
		final int creationDiff = Dates.getDateDiffInMinutes(now, creationDate);
		if (creationDiff < 0) {
			return true;
		}

		// check the difference between now and the last usage
		final int nowDiff = Dates
				.getDateDiffInMinutes(now, this.lastAccessDate);

		return nowDiff >= timeoutInMin;
	}

	/**
	 * Determines the amount of times left within the session, if a session has
	 * the specified {@code timeout}.
	 * 
	 * @param timeoutInMin
	 *            the timeout of the session in minutes
	 * 
	 * @return the amount of minutes left until the session should be invalid,
	 *         if {@code 0} the session will be invalidated or is already by the
	 *         {@code SessionManager}
	 */
	public int getLeftTimeoutInMin(final int timeoutInMin) {
		if (isTimedOut(timeoutInMin)) {
			return 0;
		} else {
			final Date now = provider.now();
			final int nowDiff = Dates.getDateDiffInMinutes(now,
					this.lastAccessDate);
			return Math.min(timeoutInMin, Math.max(0, timeoutInMin - nowDiff));
		}
	}

	/**
	 * Gets the date when the session was created.
	 * 
	 * @return the date the session was created
	 */
	public Date getCreationDate() {
		return creationDate;
	}

	/**
	 * Gets the date when the session was accessed last.
	 * 
	 * @return the date the session was accessed last
	 * 
	 * @see #markAsUsed()
	 */
	public Date getLastAccessTime() {
		return lastAccessDate;
	}

	@Override
	public String toString() {
		return getId()
				+ " of user '"
				+ getUsername()
				+ "' ("
				+ Dates.createStringFromDate(getCreationDate(),
						"dd.MM.yyyy HH:mm:ss")
				+ ", "
				+ Dates.createStringFromDate(lastAccessDate,
						"dd.MM.yyyy HH:mm:ss") + ")";
	}
}
