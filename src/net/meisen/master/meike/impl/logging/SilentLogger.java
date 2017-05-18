package net.meisen.master.meike.impl.logging;

/**
 * Logger that does not write any output.
 */
public class SilentLogger implements ILogger {
    @Override
    public void log(String message) {
        // do nothing.
    }
}
