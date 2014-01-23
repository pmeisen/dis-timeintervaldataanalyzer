package net.meisen.dissertation.config.xslt;

import net.meisen.dissertation.model.data.Descriptor;
import net.meisen.general.sbconfigurator.config.transformer.XsltImportResolver;

/**
 * Implementation of a {@code XsltURIResolver} which searches for xslt files
 * available for {@code Descriptor}.
 * 
 * @author pmeisen
 * 
 */
public class DescriptorsXsltResolver extends XsltImportResolver {
	
	@Override
	public String getProtocol() {
		return "descriptors";
	}

	@Override
	protected Class<?> getBaseClass() {
		return Descriptor.class;
	}
}
