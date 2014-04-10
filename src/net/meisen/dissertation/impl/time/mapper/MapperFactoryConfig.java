package net.meisen.dissertation.impl.time.mapper;

import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.HashSet;
import java.util.Set;

import net.meisen.dissertation.model.time.mapper.BaseMapper;
import net.meisen.dissertation.model.time.mapper.IMapperFactoryConfig;

/**
 * A {@code MapperFactoryConfig} is used to configure a {@code MapperFactory}.
 * 
 * @author pmeisen
 * 
 */
public class MapperFactoryConfig implements IMapperFactoryConfig {
	private Set<Class<? extends BaseMapper<?>>> mapper = new HashSet<Class<? extends BaseMapper<?>>>();

	/**
	 * The default constructor.
	 */
	public MapperFactoryConfig() {
		this(null);
	}

	/**
	 * Constructor used to inherit the mappers from another {@code config}.
	 * 
	 * @param config
	 *            the configuration to inherit the mappers from
	 */
	public MapperFactoryConfig(final IMapperFactoryConfig config) {
		removeAllMapper();

		if (config != null && config instanceof MapperFactoryConfig) {
			addMapper(((MapperFactoryConfig) config).mapper);
		}
	}

	/**
	 * Adds the defined {@code clazzes} as {@code BaseMapper}.
	 * 
	 * @param clazzes
	 *            the {@code BaseMapper} classes to be added
	 */
	public void setMapper(
			final Collection<Class<? extends BaseMapper<?>>> clazzes) {
		addMapper(clazzes);
	}

	/**
	 * Gets all the defined mappers.
	 * 
	 * @return all the defined mappers
	 */
	public Collection<Class<? extends BaseMapper<?>>> getMapper() {
		return Collections.unmodifiableSet(mapper);
	}

	/**
	 * Adds the defined {@code clazzes} as {@code BaseMapper}.
	 * 
	 * @param clazzes
	 *            the {@code BaseMapper} classes to be added
	 */
	public void addMapper(
			final Collection<Class<? extends BaseMapper<?>>> clazzes) {
		this.mapper.addAll(clazzes);
	}

	/**
	 * Add the defined {@code clazz} as {@code BaseMapper}.
	 * 
	 * @param clazz
	 *            the {@code BaseMapper} class to be added
	 */
	public void addMapper(final Class<? extends BaseMapper<?>> clazz) {
		this.mapper.add(clazz);
	}

	/**
	 * Adds the defined {@code clazzes} as {@code BaseMapper}.
	 * 
	 * @param clazzes
	 *            the {@code BaseMapper} classes to be added
	 */
	public void addMapper(final Class<? extends BaseMapper<?>>... clazzes) {
		this.mapper.addAll(Arrays.asList(clazzes));
	}

	/**
	 * Removes all the defined mappers and adds the default mappers.
	 */
	public void removeAllMapper() {
		this.mapper.clear();

		// add the defaults
		addMapper(DateMapper.class);
		addMapper(LongMapper.class);
	}

	@Override
	public String toString() {
		return this.mapper.toString();
	}
}
