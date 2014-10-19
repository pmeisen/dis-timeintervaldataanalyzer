package net.meisen.dissertation.server.session;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import java.util.Date;

import net.meisen.dissertation.server.sessions.Session;
import net.meisen.dissertation.server.sessions.Session.IDateProvider;

import org.junit.Test;

/**
 * Tests the implemenation of a {@code Session}.
 * 
 * @author pmeisen
 * 
 */
public class TestSession {

	/**
	 * Tests the timeout of a session.
	 */
	@Test
	public void testTimeout() {

		Session s = new Session("philipp", new IDateProvider() {
			int i = 0;

			@Override
			public Date now() {
				if (i == 0) {
					i++;
					return new Date();
				} else {
					return new Date(new Date().getTime() + 30 * 60000);
				}
			}
		});

		assertFalse(s.isTimedOut(45));
		assertFalse(s.isTimedOut(31));
		assertTrue(s.isTimedOut(30));
		assertTrue(s.isTimedOut(15));
		assertTrue(s.isTimedOut(29));
	}
}
