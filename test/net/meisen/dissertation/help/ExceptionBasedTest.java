package net.meisen.dissertation.help;

import java.util.Locale;

import net.meisen.general.genmisc.exceptions.ForwardedRuntimeException;

import org.hamcrest.Description;
import org.junit.After;
import org.junit.Before;
import org.junit.Rule;
import org.junit.internal.matchers.TypeSafeMatcher;
import org.junit.rules.ExpectedException;

/**
 * A test which uses and validates exceptions by it's message has to make sure
 * that the {@code Location} is defined. This class sets the {@code Locale} to
 * {@link Locale#US} and resets it at the end of the test.
 * 
 * @author pmeisen
 * 
 */
public class ExceptionBasedTest {

	/**
	 * Matcher used to match {@code ForwardedRuntimeException} instances.
	 * 
	 * @author pmeisen
	 * 
	 */
	public static class ForwardExceptionMatcher extends
			TypeSafeMatcher<ForwardedRuntimeException> {
		private final String expectedNumber;
		private final String expectedClass;

		/**
		 * Constructor specifying the {@code clazz} and the {@code number} of
		 * the expected exception.
		 * 
		 * @param clazz
		 *            the class
		 * @param number
		 *            the number
		 */
		public ForwardExceptionMatcher(final Class<?> clazz, final int number) {
			expectedNumber = "Number: '" + number + "'";
			expectedClass = "Exception '" + clazz.getName() + "'";
		}

		@Override
		public void describeTo(final Description description) {
			description.appendText(expectedClass + ", " + expectedNumber);
		}

		@Override
		public boolean matchesSafely(final ForwardedRuntimeException item) {
			return item.toString().contains(expectedNumber)
					&& item.toString().contains(expectedClass);
		}
	}

	/**
	 * Rule to evaluate exceptions
	 */
	@Rule
	public ExpectedException thrown = ExpectedException.none();

	private Locale oldDefault;

	/**
	 * Initializes the {@code transformer} to point to the correct {@code xslt}.
	 * Furthermore it ensures that we have {@code Locale.US} so that comparisons
	 * of errors will fit.
	 */
	@Before
	public void initLocale() {
		oldDefault = Locale.getDefault();
		Locale.setDefault(Locale.US);
	}

	/**
	 * CleansUp by resetting the Locale.
	 */
	@After
	public void cleanUpLocale() {
		Locale.setDefault(oldDefault);
	}
}
