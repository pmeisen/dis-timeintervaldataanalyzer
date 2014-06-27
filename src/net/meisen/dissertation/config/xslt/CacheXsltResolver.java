package net.meisen.dissertation.config.xslt;

import net.meisen.dissertation.model.cache.IBitmapIdCache;
import net.meisen.dissertation.model.cache.ICache;
import net.meisen.dissertation.model.cache.IDataRecordCache;
import net.meisen.dissertation.model.cache.IIdentifierCache;
import net.meisen.dissertation.model.cache.IMetaDataCache;
import net.meisen.general.sbconfigurator.config.transformer.XsltImportResolver;

/**
 * Xslt resolver used for the extensions of {@code Cache} instances.
 * 
 * @author pmeisen
 * 
 * @see IMetaDataCache
 * @see IDataRecordCache
 * @see IIdentifierCache
 * @see IBitmapIdCache
 * 
 */
public class CacheXsltResolver extends XsltImportResolver {

	@Override
	public String getProtocol() {
		return "cache";
	}

	@Override
	protected Class<?> getBaseClass() {
		return ICache.class;
	}
}