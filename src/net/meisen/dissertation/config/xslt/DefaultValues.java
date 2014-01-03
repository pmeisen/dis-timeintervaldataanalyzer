package net.meisen.dissertation.config.xslt;

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Map.Entry;

import net.meisen.dissertation.data.IIdsFactory;
import net.meisen.dissertation.data.impl.dataretriever.DbDataRetriever;
import net.meisen.dissertation.data.impl.dataretriever.RandomDataRetriever;
import net.meisen.dissertation.data.impl.descriptors.DescriptorsFactory;
import net.meisen.dissertation.data.impl.descriptors.DoubleDescriptor;
import net.meisen.dissertation.data.impl.descriptors.GeneralDescriptor;
import net.meisen.dissertation.data.impl.descriptors.IntegerDescriptor;
import net.meisen.dissertation.data.impl.descriptors.LongDescriptor;
import net.meisen.dissertation.data.impl.idfactories.IntegerIdsFactory;
import net.meisen.dissertation.data.impl.indexes.IndexedCollectionFactory;
import net.meisen.dissertation.data.impl.resources.ResourcesFactory;

/**
 * This class presents the default values used within the XSLT process.
 * 
 * @author pmeisen
 * 
 */
public class DefaultValues {

	/**
	 * The name of the module created by the XSLT process, which contains the
	 * created model.
	 * 
	 * @return name of the module created by the XSLT process
	 */
	public static String getGeneratedModuleName() {
		return "metaModelData";
	}

	/**
	 * Gets the default {@code IIndexedCollectionFactory} to be used.
	 * 
	 * @return the default {@code IIndexedCollectionFactory} to be used
	 */
	public static String getIndexedCollectionFactoryImplementation() {
		return IndexedCollectionFactory.class.getName();
	}

	/**
	 * Gets the default {@code ResourcesFactory} to be used.
	 * 
	 * @return the class of the default {@code ResourcesFactory}
	 */
	public static String getResourcesFactoryImplementation() {
		return ResourcesFactory.class.getName();
	}

	/**
	 * Gets the default {@code DescriptorsFactory} to be used.
	 * 
	 * @return the class of the default {@code DescriptorsFactory}
	 */
	public static String getDescriptorsFactoryImplementation() {
		return DescriptorsFactory.class.getName();
	}

	/**
	 * Gets the class of the {@code IdFactory} to be used for the resources.
	 * 
	 * @return the class of the {@code IdFactory} to be used for the resources
	 * 
	 * @see IIdsFactory
	 */
	public static String getResourcesFactoryIdFactory() {
		return IntegerIdsFactory.class.getName();
	}

	/**
	 * Gets the class of the {@code IdFactory} to be used for the descriptors.
	 * 
	 * @return the class of the {@code IdFactory} to be used for the descriptors
	 * 
	 * @see IIdsFactory
	 */
	public static String getDescriptorsFactoryIdFactory() {
		return IntegerIdsFactory.class.getName();
	}

	/**
	 * Get the default descriptors to be used, i.e. the map maps a class of a
	 * descriptor to the concrete {@code Descriptor} implementation.
	 * 
	 * @return the mappings between a class and a concrete {@code Descriptor}
	 *         representation
	 */
	public static Map<String, String> getDescriptors() {
		final Map<String, String> d = new LinkedHashMap<String, String>();

		// add the default mappings
		d.put(Double.class.getName(), DoubleDescriptor.class.getName());
		d.put(Long.class.getName(), LongDescriptor.class.getName());
		d.put(Integer.class.getName(), IntegerDescriptor.class.getName());
		d.put(Object.class.getName(), GeneralDescriptor.class.getName());

		return d;
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
	public static String getDefaultDataRetrieverImplementation(final String id) {

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
		d.put("rnd", RandomDataRetriever.class.getName());

		return d;
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
