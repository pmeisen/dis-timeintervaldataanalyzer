package net.meisen.dissertation.server;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.InputStream;
import java.net.URI;
import java.net.URISyntaxException;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import net.meisen.dissertation.exceptions.QueryEvaluationException;
import net.meisen.dissertation.model.parser.query.IResourceResolver;
import net.meisen.general.genmisc.exceptions.ForwardedRuntimeException;
import net.meisen.general.genmisc.types.Files;
import net.meisen.general.genmisc.types.Objects;

/**
 * A resolver used by the server to resolve locale resources.
 * 
 * @author pmeisen
 * 
 */
public class ServerResourceResolver implements IResourceResolver {
	private final static Logger LOG = LoggerFactory
			.getLogger(ServerResourceResolver.class);

	/**
	 * Supported protocols
	 */
	protected final static String RESOLVER_PROTOCOLS = "classpath|file";
	/**
	 * Syntax to be used
	 */
	protected final static String RESOLVER_SYNTAX = "(" + RESOLVER_PROTOCOLS
			+ ")" + "://[path]";

	@Override
	public InputStream resolve(final String resource)
			throws QueryEvaluationException {

		final URI uri;
		if (resource == null || resource.isEmpty()) {
			throw new ForwardedRuntimeException(QueryEvaluationException.class,
					1023, resource, RESOLVER_SYNTAX);
		} else {

			try {
				uri = new URI(resource);
			} catch (final URISyntaxException e) {
				throw new ForwardedRuntimeException(
						QueryEvaluationException.class, 1023, e, resource,
						RESOLVER_SYNTAX);
			}
		}

		// validate the uri
		if (!Objects.equals(uri.getFragment(), null)
				|| !Objects.equals(uri.getPort(), -1)
				|| !Objects.equals(uri.getQuery(), null)
				|| !Objects.equals(uri.getUserInfo(), null)) {
			throw new ForwardedRuntimeException(QueryEvaluationException.class,
					1024, resource, uri, RESOLVER_SYNTAX);
		} else if (!uri.getScheme().matches("(?i)" + RESOLVER_PROTOCOLS)) {
			throw new ForwardedRuntimeException(QueryEvaluationException.class,
					1025, resource, uri.getScheme(), RESOLVER_SYNTAX);
		} else if (!Objects.equals(uri.getAuthority(), uri.getHost())) {
			throw new ForwardedRuntimeException(QueryEvaluationException.class,
					1024, resource, uri, RESOLVER_SYNTAX);
		}

		// get the needed values
		final String protocol = uri.getScheme();
		final String path = uri.getHost() + "/" + uri.getPath().substring(1);

		// find the resource depending on the protocol
		if (protocol.equalsIgnoreCase("classpath")) {
			final InputStream res = getClass().getResourceAsStream("/" + path);
			if (res == null) {
				throw new ForwardedRuntimeException(
						QueryEvaluationException.class, 1026, resource);
			} else {
				if (LOG.isTraceEnabled()) {
					LOG.trace("Loaded resource '" + resource
							+ "' from classpath '" + path + "'.");
				}
				return res;
			}
		} else if (protocol.equalsIgnoreCase("file")) {
			final File file = new File(path);

			if (!file.exists() || !file.isFile() || !file.canRead()) {
				throw new ForwardedRuntimeException(
						QueryEvaluationException.class, 1026,
						Files.getCanonicalPath(file));
			}

			try {
				if (LOG.isTraceEnabled()) {
					LOG.trace("Loaded resource '" + resource
							+ "' from file-system '" + path + "'.");
				}

				return new FileInputStream(file);
			} catch (final FileNotFoundException e) {
				throw new ForwardedRuntimeException(
						QueryEvaluationException.class, 1026,
						Files.getCanonicalPath(file));
			}
		} else {
			throw new ForwardedRuntimeException(QueryEvaluationException.class,
					1025, resource, protocol, RESOLVER_SYNTAX);
		}
	}
}
