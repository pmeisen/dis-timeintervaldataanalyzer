package net.meisen.dissertation.help;


/**
 * A thread which can handle exceptions.
 * 
 * @author pmeisen
 * 
 */
public abstract class ThreadForTesting extends Thread {
	private Throwable ex = null;

	/**
	 * Default constructor.
	 */
	public ThreadForTesting() {
		super();
	}

	/**
	 * Constructor used to specify the name of the thread.
	 * 
	 * @param name
	 *            the name of the thread
	 */
	public ThreadForTesting(final String name) {
		super(name);
	}

	@Override
	public void run() {
		try {
			_run();
		} catch (final Throwable ex) {
			this.ex = ex;
		} finally {
			try {
				cleanUp();
			} catch (final Throwable ex) {
				this.ex = this.ex == null ? ex : this.ex;
			}
		}
	}

	/**
	 * Method which is called prior to any exception and finalization. Can be
	 * used to do thread dependent clean-up.
	 * 
	 * @throws Throwable
	 *             if an exception is thrown during clean-up
	 */
	protected void cleanUp() throws Throwable {
		// nothing to do

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
