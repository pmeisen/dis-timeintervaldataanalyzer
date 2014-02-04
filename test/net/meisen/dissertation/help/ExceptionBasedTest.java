package net.meisen.dissertation.help;

import java.util.Locale;

import org.junit.After;
import org.junit.Before;
import org.junit.Rule;
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
