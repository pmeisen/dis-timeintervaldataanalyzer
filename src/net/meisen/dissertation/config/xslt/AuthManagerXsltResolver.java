package net.meisen.dissertation.config.xslt;

import net.meisen.dissertation.model.auth.IAuthManager;
import net.meisen.general.sbconfigurator.config.transformer.XsltImportResolver;

/**
 * Implementation of a {@code XsltURIResolver} which searches for xslt files
 * available for {@code AuthManager}.
 * 
 * @author pmeisen
 * 
 * @see IAuthManager
 * 
 */
public class AuthManagerXsltResolver extends XsltImportResolver {

	@Override
	public String getProtocol() {
		return "authmanager";
	}

	@Override
	protected Class<?> getBaseClass() {
		return IAuthManager.class;
	}
}
