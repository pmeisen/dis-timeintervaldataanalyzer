package net.meisen.dissertation.help;


/**
 * A thread which can handle exceptions.
 * 
 * @author pmeisen
 * 
 */
public abstract class ThreadForTesting extends Thread {
	private Throwable ex = null;

	@Override
	public void run() {
		try {
			_run();
		} catch (final Throwable ex) {
			this.ex = ex;
		}
	}

	/**
	 * The concrete implementation of the run.
	 * 
	 * @throws Throwable
	 *             exception might be thrown
	 */
	public abstract void _run() throws Throwable;

	/**
	 * Gets the exception thrown, if none was thrown {@code null} will be
	 * returned.
	 * 
	 * @return the exception thrown
	 */
	public Throwable getException() {
		return ex;
	}
	
	/**
	 * Checks if an exception was thrown and rethrows it. 
	 */
	public void validate() {
		if (getException() instanceof AssertionError) {
			throw (AssertionError) getException();
		} else if (getException() instanceof RuntimeException) {
			throw (RuntimeException) getException();
		} else if (getException() != null) {
			throw new RuntimeException(getException());
		}
	}
}
