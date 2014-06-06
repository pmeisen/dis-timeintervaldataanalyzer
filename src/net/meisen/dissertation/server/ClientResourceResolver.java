package net.meisen.dissertation.server;

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.io.InputStream;

import net.meisen.dissertation.exceptions.QueryEvaluationException;
import net.meisen.dissertation.model.parser.query.IResourceResolver;
import net.meisen.dissertation.server.Protocol.WrappedException;
import net.meisen.general.genmisc.exceptions.ForwardedRuntimeException;

public class ClientResourceResolver implements IResourceResolver {

	private final Protocol protocol; 

	public ClientResourceResolver(final Protocol protocol) {
		this.protocol = protocol;
	}

	@Override
	public InputStream resolve(final String resource) {
		try {

			// tell the client that we need a resource
			this.protocol.writeResourceDemand(resource);

			// wait for the resource
			final byte[] resourceAsBytes = this.protocol.readResource();

			// return the resource
			return new ByteArrayInputStream(resourceAsBytes);
		} catch (final IOException e) {
			// TODO fix number 10000
			throw new ForwardedRuntimeException(QueryEvaluationException.class,
					10000, e, resource);
		} catch (final WrappedException e) {
			// TODO fix number 10001
			throw new ForwardedRuntimeException(QueryEvaluationException.class,
					10001, e.getMessage());
		}
	}
}
