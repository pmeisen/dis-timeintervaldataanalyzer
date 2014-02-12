package net.meisen.dissertation.impl.time.mapper;

import net.meisen.dissertation.model.time.mapper.BaseMapperFactory;

/**
 * The default implementation of a {@code BaseMapperFactory} which registers the
 * default {@code Mappers}, i.e.:
 * <ul>
 * <li>{@code DateMapper}</li>
 * <li>{@code LongMapper}</li>
 * </ul>
 * 
 * @author pmeisen
 * 
 * @see DateMapper
 * @see LongMapper
 * 
 */
public class MapperFactory extends BaseMapperFactory {

	/**
	 * Default constructor
	 */
	public MapperFactory() {
		addMapper(DateMapper.class);
		addMapper(LongMapper.class);
	}
}
