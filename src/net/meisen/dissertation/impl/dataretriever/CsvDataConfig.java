package net.meisen.dissertation.impl.dataretriever;

import net.meisen.dissertation.model.dataretriever.IDataRetrieverConfig;

/**
 * The configuration of a {@code CsvDataRetriever}.
 * 
 * @author pmeisen
 * 
 */
public class CsvDataConfig implements IDataRetrieverConfig {
	private String file;
	private String separator;
	private boolean classpath;

	/**
	 * Default constructor.
	 */
	public CsvDataConfig() {
		this.separator = ",";
		this.classpath = false;
		this.file = null;
	}

	/**
	 * Gets the specified location of the file to read.
	 * 
	 * @return the specified location of the file to read
	 */
	public String getFile() {
		return file;
	}

	/**
	 * Sets the location of the file to read.
	 * 
	 * @param file
	 *            the location of the file to read
	 */
	public void setFile(String file) {
		this.file = file;
	}

	/**
	 * Checks if the file should be loaded from classpath and if the location is
	 * a classpath location.
	 * 
	 * @return {@code true} if the file is loaded from classpath, otherwise
	 *         {@code false}
	 */
	public boolean isClasspath() {
		return classpath;
	}

	/**
	 * Defines if the file should be loaded from classpath and if the location
	 * is a classpath location.
	 * 
	 * @param classpath
	 *            {@code true} if the file should be loaded from classpath,
	 *            otherwise {@code false}
	 */
	public void setClasspath(boolean classpath) {
		this.classpath = classpath;
	}

	@Override
	public String toString() {
		return file + " (" + classpath + ", " + separator + ")";
	}

	/**
	 * Gets the separator specified for the file.
	 * 
	 * @return the separator specified for the file
	 */
	public String getSeparator() {
		return separator;
	}

	/**
	 * Sets the separator of the file.
	 * 
	 * @param separator
	 *            the separator of the file
	 */
	public void setSeparator(String separator) {
		this.separator = separator;
	}
}
