package net.meisen.dissertation.server;

import java.net.Socket;
import java.net.SocketException;

import net.meisen.dissertation.config.xslt.DefaultValues;
import net.meisen.dissertation.exceptions.TidaServerListenerException;
import net.meisen.dissertation.impl.parser.query.QueryFactory;
import net.meisen.general.server.api.impl.BaseListener;
import net.meisen.general.server.settings.pojos.Connector;
import net.meisen.general.server.settings.pojos.Extension;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;

/**
 * Listener used to handle queries.
 * 
 * @author pmeisen
 * 
 */
public class TidaServerListener extends BaseListener {
	private final int DEFAULT_TIMEOUTINMS = 30 * 60 * 1000;

	/**
	 * The name under which the listener is registered
	 */
	public static final String NAME = "TSQL";

	@Autowired
	@Qualifier(DefaultValues.QUERYFACTORY_ID)
	private QueryFactory queryFactory;

	private int timeoutInMs = -1;

	@Override
	public void initialize(final Connector c) {
		super.initialize(c);

		final Extension client = c.getExtension("client");
		if (client != null) {
			final int timeoutInS = client.<Integer> getProperty("timeout");
			timeoutInMs = Math.max(0, timeoutInS * 1000);
		} else {
			timeoutInMs = DEFAULT_TIMEOUTINMS;
		}
	}

	@Override
	protected Thread createWorkerThread(final Socket socket) {
		try {
			socket.setSoTimeout(timeoutInMs);
		} catch (final SocketException e) {
			getExceptionRegistry().throwException(
					TidaServerListenerException.class, 1000, timeoutInMs);
		}

		return new RequestHandlerThread(socket, queryFactory, getExceptionRegistry());
	}

	@Override
	public String toString() {
		return NAME + (getPort() == -1 ? "" : " (" + getPort() + ")");
	}
}
