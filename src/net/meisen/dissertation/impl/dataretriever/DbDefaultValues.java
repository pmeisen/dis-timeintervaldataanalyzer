package net.meisen.dissertation.impl.dataretriever;

/**
 * The default values used for the database if none is defined in the XML.
 * 
 * @author pmeisen
 * 
 */
public class DbDefaultValues {

	/**
	 * Gets the default language to be used if none is defined.
	 * 
	 * @return the default language to be used if none is defined
	 */
	public static String getDefaultLanguage() {
		return "sql";
	}
}
