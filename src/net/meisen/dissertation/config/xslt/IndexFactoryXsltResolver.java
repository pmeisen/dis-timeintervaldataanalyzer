package net.meisen.dissertation.config.xslt;

import net.meisen.dissertation.model.indexes.BaseIndexFactory;
import net.meisen.general.sbconfigurator.config.transformer.XsltImportResolver;

/**
 * Xslt resolver used for the extensions of {@code IndexFactory} instances.
 * 
 * @author pmeisen
 * 
 */
public class IndexFactoryXsltResolver extends XsltImportResolver {

	@Override
	public String getProtocol() {
		return "indexFactory";
	}

	@Override
	protected Class<?> getBaseClass() {
		return BaseIndexFactory.class;
	}
}