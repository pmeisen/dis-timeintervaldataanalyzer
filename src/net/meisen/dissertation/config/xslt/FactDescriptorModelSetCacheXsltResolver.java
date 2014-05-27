package net.meisen.dissertation.config.xslt;

import net.meisen.dissertation.model.cache.IFactDescriptorModelSetCache;
import net.meisen.general.sbconfigurator.config.transformer.XsltImportResolver;

/**
 * Xslt resolver used for the extensions of {@code FactDescriptorModelSetCache}
 * instances.
 * 
 * @author pmeisen
 * 
 * @see IFactDescriptorModelSetCache
 * 
 */
public class FactDescriptorModelSetCacheXsltResolver extends XsltImportResolver {

	@Override
	public String getProtocol() {
		return "factdescriptormodelsetcache";
	}

	@Override
	protected Class<?> getBaseClass() {
		return IFactDescriptorModelSetCache.class;
	}
}