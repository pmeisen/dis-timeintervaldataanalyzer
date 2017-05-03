package net.meisen.master.meike.impl.logging;

import java.util.concurrent.Callable;

/**
 * Can be used to log performance measures.
 */
public class PerformanceLogger implements ILogger {
    @Override
    public void log(final String message) {
        System.out.println(message);
    }

    public void logTiming(final String message, final Runnable runnable) {
        final long startTime = System.currentTimeMillis();
        runnable.run();
        final long endTime = System.currentTimeMillis();
        this.log(message + ":\t" + (endTime - startTime) + " milliseconds");
    }

    public <T> T logTiming(final String message, final Callable<T> callable) {
        final long startTime = System.currentTimeMillis();
        try {
            final T result = callable.call();
            final long endTime = System.currentTimeMillis();
            this.log(message + ":\t" + (endTime - startTime) + " milliseconds");
            return result;
        } catch (final Exception exception) {
            throw new IllegalStateException("Callable threw an exception", exception);
        }
    }
}
