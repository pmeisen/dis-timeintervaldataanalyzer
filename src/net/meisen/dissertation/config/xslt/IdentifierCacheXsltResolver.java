package net.meisen.dissertation.config.xslt;

import net.meisen.dissertation.model.cache.IIdentifierCache;
import net.meisen.general.sbconfigurator.config.transformer.XsltImportResolver;

/**
 * Xslt resolver used for the extensions of {@code IdentifierCache} instances.
 * 
 * @author pmeisen
 * 
 * @see IIdentifierCache
 * 
 */
public class IdentifierCacheXsltResolver extends XsltImportResolver {

	@Override
	public String getProtocol() {
		return "identifiercache";
	}

	@Override
	protected Class<?> getBaseClass() {
		return IIdentifierCache.class;
	}
}