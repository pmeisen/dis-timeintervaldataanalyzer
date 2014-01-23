package net.meisen.dissertation.config.xslt;

import net.meisen.dissertation.model.dataretriever.BaseDataRetriever;
import net.meisen.general.sbconfigurator.config.transformer.XsltImportResolver;

/**
 * Implementation of a {@code XsltURIResolver} which searches for xslt files
 * available for {@code DateRetriever}.
 * 
 * @author pmeisen
 * 
 */
public class DataRetrieverXsltResolver extends XsltImportResolver {

	@Override
	public String getProtocol() {
		return "dataretriever";
	}

	@Override
	protected Class<?> getBaseClass() {
		return BaseDataRetriever.class;
	}
}
