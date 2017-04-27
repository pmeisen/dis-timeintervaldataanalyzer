package net.meisen.master.meike.impl.logging;

/**
 * Simplest logger for printing to the console.
 */
public class SimpleConsoleLogger implements ILogger {
    @Override
    public void log(final String message) {
        System.out.println(message);
    }
}
