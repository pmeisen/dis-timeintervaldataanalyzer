package net.meisen.dissertation.config.xslt;

import net.meisen.dissertation.model.cache.IBitmapIdCache;
import net.meisen.general.sbconfigurator.config.transformer.XsltImportResolver;

/**
 * Xslt resolver used for the extensions of {@code ICache} instances.
 * 
 * @author pmeisen
 * 
 * @see IBitmapIdCache
 * 
 */
public class BitmapIdCacheXsltResolver extends XsltImportResolver {

	@Override
	public String getProtocol() {
		return "bitmapidcache";
	}

	@Override
	protected Class<?> getBaseClass() {
		return IBitmapIdCache.class;
	}
}