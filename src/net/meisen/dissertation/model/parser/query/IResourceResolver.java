package net.meisen.dissertation.model.parser.query;

import java.io.InputStream;

import net.meisen.dissertation.server.CancellationException;
import net.meisen.general.genmisc.exceptions.ForwardedRuntimeException;

/**
 * A resolver used to resolve specific resources for a query.
 * 
 * @author pmeisen
 * 
 */
public interface IResourceResolver {

	/**
	 * Resolves the specified {@code resource} and provides a
	 * {@code InputStream} to retrieve it. A {@code ForwardedRuntimeException}
	 * should be thrown if the resource is not available.
	 * 
	 * @param resource
	 *            the identifier of the resource to be retrieved
	 * 
	 * @return the {@code InputStream} to the resource
	 * 
	 * @throws CancellationException
	 *             if the resolving was cancelled
	 * @throws ForwardedRuntimeException
	 *             if the resource cannot be resolved
	 * 
	 * @see ForwardedRuntimeException
	 */
	public InputStream resolve(final String resource)
			throws CancellationException, ForwardedRuntimeException;
}
