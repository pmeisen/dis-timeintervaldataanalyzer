package net.meisen.dissertation.data.impl.loader;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.InputStream;

import net.meisen.dissertation.config.xslt.DefaultValues;
import net.meisen.dissertation.models.impl.data.MetaDataModel;
import net.meisen.general.genmisc.exceptions.registry.IExceptionRegistry;
import net.meisen.general.sbconfigurator.api.IConfiguration;
import net.meisen.general.sbconfigurator.api.IModuleHolder;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;

public class XmlMetaDataModelLoader {

	@Autowired
	@Qualifier(DefaultValues.EXCEPTIONREGISTRY_ID)
	private IExceptionRegistry exceptionRegistry;

	@Autowired(required = true)
	@Qualifier("coreConfiguration")
	private IConfiguration configuration;

	public MetaDataModel loadXml(final File file) {
		try {
			return loadXml(new FileInputStream(file));
		} catch (FileNotFoundException e) {
			// TODO Auto-generated catch block
			return null;
		}
	}

	public MetaDataModel loadXml(final String classPathXmlFile) {
		return loadXml(getClass().getResourceAsStream(classPathXmlFile));
	}

	public MetaDataModel loadXml(final InputStream xmlFile) {
		final InputStream inputStream = getClass().getResourceAsStream(
				"/net/meisen/dissertation/config/fullModel.xml");
		final IModuleHolder moduleHolder = configuration.loadDelayed(
				"tidaModelBeans", inputStream);
		final MetaDataModel m = moduleHolder
				.getModule(DefaultValues.METADATAMODEL_ID);

		return m;
	}
}
