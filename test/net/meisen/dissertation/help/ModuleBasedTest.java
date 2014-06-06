package net.meisen.dissertation.help;

import static org.junit.Assert.assertTrue;

import java.io.File;
import java.io.InputStream;

import net.meisen.dissertation.config.xslt.DefaultValues;
import net.meisen.general.genmisc.types.Files;
import net.meisen.general.sbconfigurator.api.IConfiguration;
import net.meisen.general.sbconfigurator.api.IModuleHolder;
import net.meisen.general.sbconfigurator.helper.SpringHelper;
import net.meisen.general.sbconfigurator.runners.JUnitConfigurationRunner;

import org.junit.After;
import org.junit.AfterClass;
import org.junit.runner.RunWith;
import org.springframework.beans.FatalBeanException;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;

/**
 * A test which includes a {@code ModuleHolder}, used to load a specific module.
 * The {@code ModuleHolder} is released at the end of each test. Use the
 * {@link #setModulesHolder(String)} to initialize a {@code ModuleHolder}
 * instance within your test. Use the {@link #modulesHolder} than to access the
 * {@code ModuleHolder}.
 * 
 * @author pmeisen
 * 
 * @see IModuleHolder
 * 
 */
@RunWith(JUnitConfigurationRunner.class)
public class ModuleBasedTest extends ExceptionBasedTest {

	/**
	 * The wired {@code Configuration}.
	 */
	@Autowired(required = true)
	@Qualifier("coreConfiguration")
	protected IConfiguration configuration;

	/**
	 * The {@code ModuleHolder} please call {@link #setModulesHolder(String)}
	 * prior to using this field.
	 */
	protected IModuleHolder modulesHolder;

	/**
	 * Creates a {@code ModuleHolder} from the specified {@code xml}.
	 * 
	 * @param xml
	 *            the XML to load the {@code ModuleHolder} from
	 */
	protected void setModulesHolder(final String xml) {
		final InputStream res = getClass().getResourceAsStream(xml);
		if (modulesHolder != null) {
			modulesHolder.release();
		}

		try {
			modulesHolder = configuration.loadDelayed("tidaXsltModelLoader",
					res);
		} catch (final FatalBeanException e) {
			final RuntimeException innerE = SpringHelper
					.getNoneSpringBeanException(e, RuntimeException.class);

			if (innerE == null) {
				throw e;
			} else {
				throw innerE;
			}
		}
	}

	/**
	 * CleansUp by releasing the {@code ModulesHolder} and shutting down the
	 * {@code Db}.
	 */
	@After
	public void cleanUpModules() {
		if (modulesHolder != null) {
			modulesHolder.release();
			modulesHolder = null;
		}
	}

	/**
	 * After the complete tests remove the system's directory.
	 */
	@AfterClass
	public static void removeDefaultLocation() {
		final String location = DefaultValues.getDefaultLocation();
		if (!location.isEmpty() && !".".equals(location)) {
			assertTrue(Files.deleteDir(new File(location)));
		}
	}
}
