package net.meisen.dissertation.config.xslt;

import net.meisen.dissertation.model.cache.IBitmapCache;
import net.meisen.general.sbconfigurator.config.transformer.XsltImportResolver;

/**
 * Xslt resolver used for the extensions of {@code BitmapCache} instances.
 * 
 * @author pmeisen
 * 
 * @see IBitmapCache
 * 
 */
public class BitmapCacheXsltResolver extends XsltImportResolver {

	@Override
	public String getProtocol() {
		return "bitmapcache";
	}

	@Override
	protected Class<?> getBaseClass() {
		return IBitmapCache.class;
	}
}