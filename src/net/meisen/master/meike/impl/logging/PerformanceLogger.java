package net.meisen.master.meike.impl.logging;

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
}
