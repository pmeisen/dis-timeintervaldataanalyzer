package net.meisen.dissertation.config.xslt;

import net.meisen.dissertation.model.indexes.BaseIndexedCollectionFactory;
import net.meisen.general.sbconfigurator.config.transformer.XsltImportResolver;

public class IndexedCollectionFactoryXsltResolver extends XsltImportResolver {

	@Override
	public String getProtocol() {
		return "indexedCollectionFactory";
	}

	@Override
	protected Class<?> getBaseClass() {
		return BaseIndexedCollectionFactory.class;
	}
}