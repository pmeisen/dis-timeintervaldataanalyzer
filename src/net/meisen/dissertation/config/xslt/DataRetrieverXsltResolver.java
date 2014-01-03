package net.meisen.dissertation.config.xslt;

import java.io.ByteArrayInputStream;
import java.io.File;
import java.io.InputStream;
import java.io.UnsupportedEncodingException;
import java.net.URI;
import java.util.Collection;
import java.util.HashSet;
import java.util.Set;
import java.util.regex.Pattern;

import javax.xml.transform.Source;
import javax.xml.transform.stream.StreamSource;

import net.meisen.dissertation.models.impl.dataretriever.BaseDataRetriever;
import net.meisen.general.genmisc.resources.Resource;
import net.meisen.general.genmisc.resources.ResourceInfo;
import net.meisen.general.genmisc.resources.ResourceType;
import net.meisen.general.genmisc.types.Files;
import net.meisen.general.sbconfigurator.config.transformer.XsltURIResolver;

/**
 * Implementation of a {@code XsltURIResolver} which searches for xslt files
 * available for {@code DateRetriever}.
 * 
 * @author pmeisen
 * 
 */
public class DataRetrieverXsltResolver implements XsltURIResolver {

	@Override
	public String getProtocol() {
		return "dataretriever";
	}

	@Override
	public Source resolve(final URI uri, final String path) {

		// get all the XSLTs available on the classpath
		final Collection<ResourceInfo> xsltResInfos = Resource.getResources(
				Pattern.compile("(?i).*\\.xslt"), true, false);

		// get the valid XSLTs
		final Set<String> xsltIncludes = new HashSet<String>();
		for (final ResourceInfo resInfo : xsltResInfos) {

			// transform the ResourceInfo to a Class
			final String clazz = transformToClass(resInfo);

			// check if the transformation was successful
			if (clazz == null || !isValidClass(clazz)) {
				continue;
			}

			xsltIncludes.add(createXsltPath(clazz, resInfo));
		}

		// create a resource which includes all the XSLTs
		return createSource(xsltIncludes);
	}

	/**
	 * Creates a path for the specified {@code Resource} of using the specified
	 * class as path.
	 * 
	 * @param clazz
	 *            the class to create the xslt for
	 * @param resInfo
	 *            the resource pointing to the xslt
	 * 
	 * @return the created xslt path
	 */
	protected String createXsltPath(final String clazz,
			final ResourceInfo resInfo) {
		final String ext = Files.getExtension(resInfo.getFullPath());
		return clazz.replace('.', '/') + "." + ext;
	}

	/**
	 * Creates a {@code Source} which includes all the specified xslt-files.
	 * 
	 * @param xslts
	 *            the {@code Collection} of xslts to be included
	 * 
	 * @return the created {@code Source} which includes all the specified
	 *         xslt-files
	 */
	protected Source createSource(final Collection<String> xslts) {

		String xsltFile = "";
		xsltFile += "<?xml version=\"1.0\" encoding=\"UTF-8\" ?>";
		xsltFile += System.getProperty("line.separator");
		xsltFile += "<stylesheet version=\"1.0\" xmlns=\"http://www.w3.org/1999/XSL/Transform\">";

		for (final String xslt : xslts) {
			xsltFile += "<include href=\"classpath://" + xslt + "\" />";
		}

		xsltFile += "</stylesheet>";

		// create the stream from the string
		final InputStream stream;
		try {
			stream = new ByteArrayInputStream(xsltFile.getBytes("UTF-8"));
		} catch (final UnsupportedEncodingException e) {
			// should never happen UTF8 is default
			return null;
		}

		return new StreamSource(stream);
	}

	/**
	 * Transforms the specified xslt {@code Resource} to a class, i.e. removes
	 * the ending, replaces separators.
	 * 
	 * @param xsltResInfo
	 *            the xslt resource to transform or determine the class for
	 * 
	 * @return the class which is not validated
	 * 
	 * @see #isValidClass(String)
	 */
	protected String transformToClass(final ResourceInfo xsltResInfo) {

		// get the path to the xslt with '.' instead of a separator
		final String modXsltPath;
		if (ResourceType.IN_JAR_FILE.equals(xsltResInfo.getType())) {
			final String xsltPath = xsltResInfo.getInJarPath();
			modXsltPath = xsltPath == null ? null : xsltPath.replace('/', '.');
		} else {
			final String xsltPath = xsltResInfo.getRelativePathToRoot();
			modXsltPath = xsltPath == null ? null : xsltPath.replace(
					File.separatorChar, '.');
		}

		if (modXsltPath == null) {
			return null;
		} else {
			return modXsltPath.replaceFirst("(?i).xslt$", "");
		}
	}

	/**
	 * Checks if the specified {@code clazzName} is valid considering the name
	 * and the used super-class (i.e. must be assignable from
	 * {@code BaseDataRetriever}.
	 * 
	 * @param clazzName
	 *            the class to be validated
	 * 
	 * @return {@code true} if the class is valid to be used, otherwise
	 *         {@code false}
	 */
	protected boolean isValidClass(final String clazzName) {
		if (clazzName == null) {
			return false;
		}

		try {
			final Class<?> clazz = Class.forName(clazzName);
			return BaseDataRetriever.class.isAssignableFrom(clazz);
		} catch (final ClassNotFoundException e) {
			return false;
		}
	}
}
