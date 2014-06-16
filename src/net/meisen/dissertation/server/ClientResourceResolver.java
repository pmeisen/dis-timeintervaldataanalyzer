package net.meisen.dissertation.server;

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.io.InputStream;

import net.meisen.dissertation.exceptions.QueryEvaluationException;
import net.meisen.dissertation.jdbc.protocol.Protocol;
import net.meisen.dissertation.jdbc.protocol.WrappedException;
import net.meisen.dissertation.model.parser.query.IResourceResolver;
import net.meisen.general.genmisc.exceptions.ForwardedRuntimeException;

/**
 * A {@code ResourceResolver} used to resolve resources from the connected
 * client.
 * 
 * @author pmeisen
 * 
 */
public class ClientResourceResolver implements IResourceResolver {

	private final Protocol protocol;

	/**
	 * Constructor which specifies the {@code Protocol} used to retrieve the
	 * resource.
	 * 
	 * @param protocol
	 *            the used protocol
	 */
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
			throw new ForwardedRuntimeException(QueryEvaluationException.class,
					1015, e, resource);
		} catch (final WrappedException e) {
			throw new ForwardedRuntimeException(QueryEvaluationException.class,
					1016, resource, e.getMessage());
		}
	}
}
