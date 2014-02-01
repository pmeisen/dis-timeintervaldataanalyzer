package net.meisen.dissertation.config.xslt;

import java.lang.reflect.Field;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.math.BigDecimal;
import java.math.BigInteger;
import java.util.Date;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Map.Entry;

import net.meisen.dissertation.impl.dataretriever.DbDataRetriever;
import net.meisen.dissertation.impl.dataretriever.FixedStructureDataRetriever;
import net.meisen.dissertation.impl.idfactories.IntegerIdsFactory;
import net.meisen.dissertation.impl.indexes.IndexedCollectionFactory;
import net.meisen.dissertation.model.idfactories.IIdsFactory;
import net.meisen.general.genmisc.types.Classes;
import net.meisen.general.genmisc.types.Dates;
import net.meisen.general.genmisc.types.Strings;
import net.meisen.general.sbconfigurator.api.IConfiguration;

/**
 * This class presents the default values used within the XSLT process.
 * 
 * @author pmeisen
 * 
 */
public class DefaultValues {
	/**
	 * The name of the module created by the XSLT process, which contains the
	 * created {@code MetaDataModel}.
	 */
	public static final String METADATAMODEL_ID = "metaDataModel";
	/**
	 * The name of the module created by the XSLT process, which contains the
	 * created {@code DataModel}.
	 */
	public static final String DATAMODEL_ID = "dataModel";
	/**
	 * The name of the module created by the XSLT process, which contains the
	 * created {@code TimeIntervalDataAnalyzerModel}.
	 */
	public static final String TIMEINTERVALDATAANALYZERMODEL_ID = "timeIntervalDataAnalyzerModel";
	/**
	 * The name of the module created by the XSLT process, which contains the
	 * created {@code IndexedCollectionFactory}.
	 */
	public static final String INDEXFACTORY_ID = "indexFactory";
	/**
	 * The name of the module for the exceptionRegistry
	 */
	public static final String EXCEPTIONREGISTRY_ID = IConfiguration.coreExceptionRegistryId;

	/**
	 * Access method to the id via reflection used for XSLT process.
	 * 
	 * @param id
	 *            the id to get the value for
	 * 
	 * @return the value of the static field named {@code id} of
	 *         {@code DefaultValue}
	 */
	public static String getId(final String id) {
		try {
			final Field idField = DefaultValues.class.getField(id);
			return (String) idField.get(null);
		} catch (final Exception e) {
			return null;
		}
	}

	/**
	 * Gets the default {@code BaseIndexedCollectionFactory} to be used.
	 * 
	 * @return the default {@code BaseIndexedCollectionFactory} to be used
	 */
	public static String getDefaultIndexedCollectionFactory() {
		return IndexedCollectionFactory.class.getName();
	}

	/**
	 * Gets the class of the {@code IdFactory} to be used for the descriptors.
	 * 
	 * @return the class of the {@code IdFactory} to be used for the descriptors
	 * 
	 * @see IIdsFactory
	 */
	public static String getDefaultIdFactory() {
		return IntegerIdsFactory.class.getName();
	}

	/**
	 * Determines the {@code DataRetriever} to be used for the specified
	 * {@code id}.
	 * 
	 * @param id
	 *            the id to determine a default {@code DataRetriever} for
	 * 
	 * @return the {@code Clazz} of the {@code DataRetriever} to use, can be
	 *         also {@code null} if no default is known for the specified
	 *         {@code id}
	 */
	public static String getDefaultDataRetriever(final String id) {

		// check if an id starts with the specified value
		if (id != null) {
			for (final Entry<String, String> e : getDataRetrievers().entrySet()) {
				if (id.startsWith(e.getKey() + "_")) {
					return e.getValue();
				}
			}
		}

		return null;
	}

	/**
	 * The default {@code DataRetrievers} available.
	 * 
	 * @return a map which associates the identifiers of the {code
	 *         DataRetrievers} with the implementation
	 */
	public static Map<String, String> getDataRetrievers() {
		final Map<String, String> d = new LinkedHashMap<String, String>();

		d.put("db", DbDataRetriever.class.getName());
		d.put("fxd", FixedStructureDataRetriever.class.getName());

		return d;
	}

	/**
	 * Determines the class to be used for the specified {@code type}.
	 * 
	 * @param type
	 *            the type to specify the class for
	 * 
	 * @return the string representation of the class to be used for the
	 *         {@code type}
	 */
	public static String determineTypeClass(final String type) {
		if ("int".equalsIgnoreCase(type)) {
			return "int";
		} else if ("long".equalsIgnoreCase(type)) {
			return "long";
		} else if ("double".equalsIgnoreCase(type)) {
			return "double";
		} else if ("date".equalsIgnoreCase(type)) {
			return Date.class.getName();
		} else if ("bigInt".equalsIgnoreCase(type)) {
			return BigInteger.class.getName();
		} else if ("bigDec".equalsIgnoreCase(type)) {
			return BigDecimal.class.getName();
		} else if (Classes.getClass(type, false) != null) {
			return type;
		} else {
			throw new IllegalArgumentException(
					"The type '"
							+ type
							+ "' is not supported please try to specify the class using the class attribute.");
		}
	}

	/**
	 * Determines the class to be used for the specified {@code value}.
	 * 
	 * @param value
	 *            the value to specify the class for
	 * 
	 * @return the string representation of the class to be used for the
	 *         {@code value}
	 */
	public static String determineValueClass(final String value) {

		if (value == null || "".equals(value.trim())) {
			return String.class.getName();
		} else if (Strings.isInteger(value) != null) {
			return "int";
		} else if (Strings.isLong(value) != null) {
			return "long";
		} else if (Strings.isDouble(value) != null) {
			return "double";
		} else if (Strings.isBigInteger(value) != null) {
			return BigInteger.class.getName();
		} else if (Strings.isBigDecimal(value) != null) {
			return BigDecimal.class.getName();
		} else if (Dates.isDate(value) != null) {
			return Date.class.getName();
		} else {
			return String.class.getName();
		}
	}

	/**
	 * Helper method for the XSLT process, which provides the result of a method
	 * defined by {@code methodName} as string with separators.
	 * 
	 * @param methodName
	 *            the method to be executed
	 * @param itemSeparator
	 *            the separator used to separate the items from each other,
	 *            whereby each item is a tuple (key and value)
	 * @param keyValueSeparator
	 *            the separator used to separate the key and the value from each
	 *            other
	 * 
	 * @return the string used to represent the descriptors
	 * 
	 * @throws NoSuchMethodException
	 *             if the specified method does not exist
	 * @throws SecurityException
	 *             if the specified method cannot be accessed
	 * @throws InvocationTargetException
	 *             if the method cannot be invoked, e.g. is not static
	 * @throws IllegalAccessException
	 *             if the method cannot be accessed
	 * @throws IllegalArgumentException
	 *             if some argument is invalid
	 */
	public static String getCsv(final String methodName,
			final String itemSeparator, final String keyValueSeparator)
			throws SecurityException, NoSuchMethodException,
			IllegalArgumentException, IllegalAccessException,
			InvocationTargetException {
		String s = "";

		final Method method = DefaultValues.class.getMethod(methodName);
		@SuppressWarnings("unchecked")
		final Map<String, String> map = (Map<String, String>) method
				.invoke(null);

		String separator = "";
		for (final Entry<String, String> e : map.entrySet()) {
			s += separator + e.getKey() + keyValueSeparator + e.getValue();
			if ("".equals(separator)) {
				separator = itemSeparator;
			}
		}

		return s;
	}
}
