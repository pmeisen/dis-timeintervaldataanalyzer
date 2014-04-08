package net.meisen.dissertation.config.xslt;

import net.meisen.dissertation.model.indexes.BaseIndexFactory;
import net.meisen.general.sbconfigurator.config.transformer.XsltImportResolver;

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