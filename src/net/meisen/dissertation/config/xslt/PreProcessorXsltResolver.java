package net.meisen.dissertation.config.xslt;

import net.meisen.dissertation.model.auth.IAuthManager;
import net.meisen.dissertation.model.dataintegration.IPreProcessor;
import net.meisen.general.sbconfigurator.config.transformer.XsltImportResolver;

/**
 * Implementation of a {@code XsltURIResolver} which searches for xslt files
 * available for {@code PreProcessor}.
 * 
 * @author pmeisen
 * 
 * @see IAuthManager
 * 
 */
public class PreProcessorXsltResolver extends XsltImportResolver {

	@Override
	public String getProtocol() {
		return "preprocessor";
	}

	@Override
	protected Class<?> getBaseClass() {
		return IPreProcessor.class;
	}
}
