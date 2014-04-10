package net.meisen.dissertation.model.time.mapper;

import java.lang.reflect.Constructor;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.SortedMap;
import java.util.TreeMap;

import net.meisen.dissertation.config.xslt.DefaultValues;
import net.meisen.dissertation.model.time.granularity.ITimeGranularity;
import net.meisen.general.genmisc.exceptions.registry.IExceptionRegistry;
import net.meisen.general.genmisc.types.Classes;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;

/**
 * An abstract implementation of a {@code MapperFactory} which is used to create
 * {@code BaseMapper}.
 * 
 * @author pmeisen
 * 
 */
public abstract class BaseMapperFactory {
	private final static Logger LOG = LoggerFactory
			.getLogger(BaseMapperFactory.class);

	/**
	 * {@code exceptionRegistry} used to fire exceptions.
	 */
	@Autowired
	@Qualifier(DefaultValues.EXCEPTIONREGISTRY_ID)
	protected IExceptionRegistry exceptionRegistry;

	private final Map<Class<?>, Class<? extends BaseMapper<?>>> mappers = new HashMap<Class<?>, Class<? extends BaseMapper<?>>>();

	/**
	 * Adds a {@code MapperClass} to the {@code Factory} so that it can be
	 * considered to create a {@code BaseMapper}.
	 * 
	 * @param mapper
	 *            the class of the {@code Mapper} to be added
	 * 
	 * @return {@code true} if it was added, otherwise {@code false}
	 */
	public boolean addMapper(final Class<? extends BaseMapper<?>> mapper) {

		if (mapper == null) {
			return false;
		} else {
			final Class<?> mappedType;
			try {
				mappedType = Classes.getGenericClass(mapper);
			} catch (final Exception e) {
				if (LOG.isWarnEnabled()) {
					LOG.warn("Cannot determine the generic type of '"
							+ mapper.getClass().getName() + "'.", e);
				}
				return false;
			}

			// add it and make sure it doesn't override anything
			if (mappers.put(mappedType, mapper) != null) {
				if (LOG.isWarnEnabled()) {
					LOG.warn("Several mappers for class '"
							+ mappedType.getName() + "' was added already.");
				}
			}
		}

		return true;
	}
	
	/**
	 * Removes all the specified mappers.
	 */
	public void removeAllMapper() {
		mappers.clear();
	}

	/**
	 * Creates a {@code Mapper} for the {@code start}, and {@code end} to be
	 * {@code null}, and the specified {@code granularity}. The {@code Mapper}
	 * also supports the additional {@code parameters}.
	 * 
	 * @param clazz
	 *            the class of the start and end
	 * @param granularity
	 *            the {@code TimeGranularity} to be used by the {@code Mapper}
	 * @param parameters
	 *            the additional parameters to be passed to the
	 *            {@code Constructor}
	 * 
	 * @return the created {@code BaseMapper}
	 */
	public BaseMapper<?> createWithNulls(final Class<?> clazz,
			final ITimeGranularity granularity, final Object... parameters) {
		return createMapper(null, null, granularity, clazz, parameters);
	}

	/**
	 * Creates a {@code Mapper} for the specified {@code start}, {@code end},
	 * and {@code granularity}. The {@code Mapper} also supports the additional
	 * {@code parameters}.
	 * 
	 * @param start
	 *            the start value
	 * @param end
	 *            the end value
	 * @param granularity
	 *            the {@code TimeGranularity} to be used by the {@code Mapper}
	 * @param parameters
	 *            the additional parameters to be passed to the
	 *            {@code Constructor}
	 * 
	 * @return the created {@code BaseMapper}
	 */
	public BaseMapper<?> createWithPrimitives(final byte start, final byte end,
			final ITimeGranularity granularity, final Object... parameters) {
		return createMapper(start, end, granularity, Byte.class, parameters);
	}

	/**
	 * Creates a {@code Mapper} for the specified {@code start}, {@code end},
	 * and {@code granularity}. The {@code Mapper} also supports the additional
	 * {@code parameters}.
	 * 
	 * @param start
	 *            the start value
	 * @param end
	 *            the end value
	 * @param granularity
	 *            the {@code TimeGranularity} to be used by the {@code Mapper}
	 * @param parameters
	 *            the additional parameters to be passed to the
	 *            {@code Constructor}
	 * 
	 * @return the created {@code BaseMapper}
	 */
	public BaseMapper<?> createWithPrimitives(final short start,
			final short end, final ITimeGranularity granularity,
			final Object... parameters) {
		return createMapper(start, end, granularity, Short.class, parameters);
	}

	/**
	 * Creates a {@code Mapper} for the specified {@code start}, {@code end},
	 * and {@code granularity}. The {@code Mapper} also supports the additional
	 * {@code parameters}.
	 * 
	 * @param start
	 *            the start value
	 * @param end
	 *            the end value
	 * @param granularity
	 *            the {@code TimeGranularity} to be used by the {@code Mapper}
	 * @param parameters
	 *            the additional parameters to be passed to the
	 *            {@code Constructor}
	 * 
	 * @return the created {@code BaseMapper}
	 */
	public BaseMapper<?> createWithPrimitives(final int start, final int end,
			final ITimeGranularity granularity, final Object... parameters) {
		return createMapper(start, end, granularity, Integer.class, parameters);
	}

	/**
	 * Creates a {@code Mapper} for the specified {@code start}, {@code end},
	 * and {@code granularity}. The {@code Mapper} also supports the additional
	 * {@code parameters}.
	 * 
	 * @param start
	 *            the start value
	 * @param end
	 *            the end value
	 * @param granularity
	 *            the {@code TimeGranularity} to be used by the {@code Mapper}
	 * @param parameters
	 *            the additional parameters to be passed to the
	 *            {@code Constructor}
	 * 
	 * @return the created {@code BaseMapper}
	 */
	public BaseMapper<?> createWithPrimitives(final long start, final long end,
			final ITimeGranularity granularity, final Object... parameters) {
		return createMapper(start, end, granularity, Long.class, parameters);
	}

	/**
	 * Creates a {@code Mapper} for the specified {@code start}, {@code end},
	 * and {@code granularity}. The {@code Mapper} also supports the additional
	 * {@code parameters}.
	 * 
	 * @param start
	 *            the start value
	 * @param end
	 *            the end value
	 * @param granularity
	 *            the {@code TimeGranularity} to be used by the {@code Mapper}
	 * @param parameters
	 *            the additional parameters to be passed to the
	 *            {@code Constructor}
	 * 
	 * @return the created {@code BaseMapper}
	 */
	public BaseMapper<?> createWithObjects(final Object start, Object end,
			final ITimeGranularity granularity, final Object... parameters) {

		// determine the class of the parameter
		final Class<?> clazz;
		if (start == null && end == null) {
			throw new IllegalArgumentException(
					"If you want to create a null-based mapper, please use createMapper(Class<?>, ITimeGranularity, Class<?>...) instead.");
		} else if (start == null) {
			clazz = end.getClass();
		} else {
			clazz = start.getClass();
		}

		return createMapper(start, end, granularity, clazz, parameters);
	}

	/**
	 * Creates a {@code Mapper} for the specified {@code start}, {@code end},
	 * and {@code granularity}. The {@code Mapper} also supports the additional
	 * {@code parameters}.
	 * 
	 * @param start
	 *            the start value
	 * @param end
	 *            the end value
	 * @param granularity
	 *            the {@code TimeGranularity} to be used by the {@code Mapper}
	 * @param clazz
	 *            the class of the mapper to be supported
	 * @param parameters
	 *            the additional parameters to be passed to the
	 *            {@code Constructor}
	 * 
	 * @return the created {@code BaseMapper}
	 */
	protected BaseMapper<?> createMapper(final Object start, final Object end,
			final ITimeGranularity granularity, final Class<?> clazz,
			final Object... parameters) {
		final Class<?>[] parameterTypes = determineParameterTypes(parameters);

		// create an ignore list
		final List<Class<? extends BaseMapper<?>>> ignoreList = new ArrayList<Class<? extends BaseMapper<?>>>();
		while (true) {
			final Constructor<? extends BaseMapper<?>> ctor = determineConstructor(
					mapClass(clazz), parameterTypes, ignoreList);

			if (ctor == null) {
				if (LOG.isWarnEnabled()) {
					LOG.warn("Couldn't find any constructor for mapper '"
							+ clazz + "'");
				}
				return null;
			}
			final Object[] wrapped = wrapObjects(start, end, granularity,
					parameters);

			try {
				return ctor.newInstance(wrapped);
			} catch (final Exception e) {
				if (LOG.isWarnEnabled()) {
					LOG.warn("Could not create a mapper for '" + clazz
							+ "' using start '" + start + "' and end '" + end
							+ "'.", e);
				}

				// try another one by ignoring the current one
				ignoreList.add(ctor.getDeclaringClass());
			}
		}
	}

	/**
	 * Determines the additional parameter-types to be supported by the
	 * {@code Constructor}.
	 * 
	 * @param parameters
	 *            the parameters to determine the type for
	 * 
	 * @return the {@code Class} instances of the parameters
	 */
	protected Class<?>[] determineParameterTypes(final Object[] parameters) {

		if (parameters == null || parameters.length < 1) {
			return new Class<?>[] {};
		} else {
			final int parametersSize = parameters.length;
			final Class<?>[] clazzes = new Class<?>[parametersSize];
			for (int i = 0; i < parametersSize; i++) {
				final Object p = parameters[i];
				clazzes[i] = p == null ? Object.class : mapClass(p.getClass());
			}

			return clazzes;
		}
	}

	/**
	 * Determines the {@code Constructor} to be used for the specified
	 * {@code startEndType} and the additional parameters. The
	 * {@code ignoreList} specifies the {@code Mappers} which should not be
	 * considered anymore.
	 * 
	 * @param startEndType
	 *            the type to be supported by the {@code Mapper}
	 * @param addParameterTypes
	 *            the additional parameters (beside {@code start}, {@code end}
	 *            and {@code granularity} needed by the constructor)
	 * @param ignoreList
	 *            the {@code Mapper} which should not be considered to be used
	 * 
	 * @return a {@code Constructor} which can be used to be called with
	 *         {@code start}, {@code end}, {@code granularity}, and the
	 *         additional parameters {@code addParameterTypes}
	 */
	@SuppressWarnings("unchecked")
	protected Constructor<? extends BaseMapper<?>> determineConstructor(
			final Class<?> startEndType, final Class<?>[] addParameterTypes,
			List<Class<? extends BaseMapper<?>>> ignoreList) {
		if (ignoreList == null) {
			ignoreList = new ArrayList<Class<? extends BaseMapper<?>>>();
		}

		// determine the mapper to be used
		Class<? extends BaseMapper<?>> mapperClass = null;
		Class<?> currentClass = startEndType;
		while (currentClass != null && mapperClass == null) {

			// get a mapper for the current class
			mapperClass = mappers.get(currentClass);

			// if it's ignored we don't have to do anything else
			if (ignoreList.contains(mapperClass)) {
				mapperClass = null;
			}

			// get the superClass and check if there is a mapper for this one
			currentClass = currentClass.getSuperclass();
		}

		// if we didn't find any mapper return
		if (mapperClass == null) {
			return null;
		}

		// determine the constructor to be called
		final SortedMap<Integer, Constructor<?>> ctors = new TreeMap<Integer, Constructor<?>>();
		for (final Constructor<?> ctor : mapperClass.getConstructors()) {
			ctors.put(weighConstructor(ctor, startEndType, addParameterTypes),
					ctor);
		}

		// get the one with the highest score to be used
		final int highest = ctors.lastKey();
		if (highest == -1) {

			// add the mapper to the ignore list, shouldn't be tested twice
			ignoreList.add(mapperClass);

			// check for another constructor
			return determineConstructor(startEndType, addParameterTypes,
					ignoreList);
		} else {
			return (Constructor<? extends BaseMapper<?>>) ctors.get(highest);
		}
	}

	/**
	 * Weighs a {@code Constructor} to be used for the purpose to be called with
	 * {@code start}, {@code end}, {@code granularity}, and the additional
	 * parameters {@code addParameterTypes}
	 * 
	 * @param ctor
	 *            the {@code Constructor} to be weighed
	 * @param type
	 *            the type of the {@code start} and {@code end} parameter
	 * @param addParameterTypes
	 *            the additional parameters to be passed to the
	 *            {@code Constructor}
	 * 
	 * @return a {@code weighing} factor, which is smaller than {@code 0} if the
	 *         {@code Constructor} cannot be used at all, otherwise values
	 *         larger than {@code 1} which classify the usefulness of the
	 *         {@code Constructor}
	 */
	protected int weighConstructor(final Constructor<?> ctor,
			final Class<?> type, final Class<?>[] addParameterTypes) {
		final Class<?>[] parameterTypes = ctor.getParameterTypes();

		final int addParameterSize = addParameterTypes == null ? 0
				: addParameterTypes.length;
		final int parameterSize = parameterTypes.length;

		if (parameterSize < 3) {
			return -1;
		} else if (parameterSize != 3 + addParameterSize) {
			return -1;
		} else {
			int weight = 0;

			if (!mapClass(parameterTypes[0]).isAssignableFrom(type)
					|| !mapClass(parameterTypes[1]).isAssignableFrom(type)
					|| !parameterTypes[2]
							.isAssignableFrom(ITimeGranularity.class)) {
				return -1;
			} else {
				weight += parameterTypes[0].equals(type) ? 2 : 1;
				weight += parameterTypes[1].equals(type) ? 2 : 1;
				weight += parameterTypes[2].equals(ITimeGranularity.class) ? 2
						: 1;
			}

			// now check the other parameters
			for (int i = 0; i < addParameterSize; i++) {
				if (mapClass(parameterTypes[i + 3]).isAssignableFrom(
						addParameterTypes[i])) {
					weight += parameterTypes[i + 3]
							.equals(addParameterTypes[i]) ? 2 : 1;
				} else {
					return -1;
				}
			}

			return weight;
		}
	}

	/**
	 * Wraps the values to one array.
	 * 
	 * @param start
	 *            the start value
	 * @param end
	 *            the end value
	 * @param granularity
	 *            the {@code TimeGranularity} to be used by the {@code Mapper}
	 * @param parameters
	 *            the additional parameters
	 * 
	 * @return the created array
	 */
	protected Object[] wrapObjects(final Object start, final Object end,
			final ITimeGranularity granularity, final Object[] parameters) {
		final int pSize = parameters == null ? 0 : parameters.length;
		final Object[] objects = new Object[3 + pSize];

		// combine the objects
		objects[0] = start;
		objects[1] = end;
		objects[2] = granularity;
		for (int i = 0; i < pSize; i++) {
			objects[i + 3] = parameters[i];
		}

		return objects;
	}

	/**
	 * Checks if the specified {@code clazz} is a primitive type.
	 * 
	 * @param clazz
	 *            the class to be checked
	 * 
	 * @return {@code true} if the specified {@code clazz} represents a
	 *         primitive one
	 */
	protected Class<?> mapClass(final Class<?> clazz) {

		if (int.class.equals(clazz)) {
			return Integer.class;
		} else if (long.class.equals(clazz)) {
			return Long.class;
		} else if (short.class.equals(clazz)) {
			return Short.class;
		} else if (byte.class.equals(clazz)) {
			return Byte.class;
		} else if (char.class.equals(clazz)) {
			return Character.class;
		} else if (boolean.class.equals(clazz)) {
			return Boolean.class;
		} else if (float.class.equals(clazz)) {
			return Float.class;
		} else if (double.class.equals(clazz)) {
			return Double.class;
		} else {
			return clazz;
		}
	}

	/**
	 * The base implementation does not support any configuration. Therefore the
	 * default implementation does nothing. If a configuration is supported this
	 * method should be overwritten.
	 * 
	 * @param config
	 *            the configuration to be used
	 */
	public void setConfig(final IMapperFactoryConfig config) {
		// nothing to do this might be implemented by the concrete class
		if (config != null) {
			if (LOG.isInfoEnabled()) {
				LOG.info("A configuration '"
						+ config
						+ "' was passed to the implementation '"
						+ getClass().getName()
						+ "' of an MapperFactory. The configuration is not used, because the base implementation does not support any configuration. Please override the appropriate method in the concrete implementation.");
			}
		}
	}
}
