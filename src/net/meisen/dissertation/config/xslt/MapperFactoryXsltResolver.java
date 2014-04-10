package net.meisen.dissertation.config.xslt;

import net.meisen.dissertation.model.time.mapper.BaseMapperFactory;
import net.meisen.general.sbconfigurator.config.transformer.XsltImportResolver;

/**
 * Xslt resolver used for the extensions of {@code MapperFactory} instances.
 * 
 * @author pmeisen
 * 
 */
public class MapperFactoryXsltResolver extends XsltImportResolver {

	@Override
	public String getProtocol() {
		return "mapperFactory";
	}

	@Override
	protected Class<?> getBaseClass() {
		return BaseMapperFactory.class;
	}
}