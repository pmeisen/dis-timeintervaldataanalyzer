package net.meisen.dissertation.config.xslt;

import net.meisen.dissertation.model.cache.IMetaDataCache;
import net.meisen.general.sbconfigurator.config.transformer.XsltImportResolver;

/**
 * Xslt resolver used for the extensions of {@code MetaDataCache} instances.
 * 
 * @author pmeisen
 * 
 * @see IMetaDataCache
 * 
 */
public class MetaDataCacheXsltResolver extends XsltImportResolver {

	@Override
	public String getProtocol() {
		return "metadatacache";
	}

	@Override
	protected Class<?> getBaseClass() {
		return IMetaDataCache.class;
	}
}