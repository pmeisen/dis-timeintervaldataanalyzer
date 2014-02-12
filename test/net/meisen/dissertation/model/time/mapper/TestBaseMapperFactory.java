package net.meisen.dissertation.model.time.mapper;

import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;

import java.util.Date;

import net.meisen.dissertation.impl.time.mapper.DateMapper;
import net.meisen.dissertation.impl.time.mapper.LongMapper;
import net.meisen.dissertation.model.time.granularity.MilliSecond;
import net.meisen.dissertation.model.time.mapper.mock.MockJustStartEndMapper;
import net.meisen.dissertation.model.time.mapper.mock.MockObjectMapper;
import net.meisen.dissertation.model.time.mapper.mock.MockSeveralConstructorsMapper;
import net.meisen.dissertation.model.time.mapper.mock.MockWithParametersMapper;

import org.junit.Test;

/**
 * Tests the implementation of the {@code BaseMapperFactory}.
 * 
 * @author pmeisen
 * 
 */
public class TestBaseMapperFactory {

	/**
	 * Tests the adding of mappers to the factory.
	 */
	@Test
	public void testAddMappers() {
		final BaseMapperFactory factory = new BaseMapperFactory() {
			// nothing more to add
		};

		// add some mappers
		assertTrue(factory.addMapper(DateMapper.class));
		assertTrue(factory.addMapper(LongMapper.class));

		assertTrue(factory.addMapper(MockObjectMapper.class));
	}

	/**
	 * Tests the simple selection of a mapper.
	 */
	@Test
	public void testCreationOfWithObjects() {
		final BaseMapperFactory factory = new BaseMapperFactory() {
			// nothing more to add
		};
		assertTrue(factory.addMapper(DateMapper.class));
		assertTrue(factory.addMapper(LongMapper.class));

		@SuppressWarnings("unchecked")
		final BaseMapper<?> mapper = (BaseMapper<Date>) factory
				.createWithObjects(new Date(), new Date(),
						MilliSecond.instance());
		assertTrue(mapper instanceof DateMapper);

	}

	/**
	 * Tests the creation using {@code null} values.
	 */
	@Test
	public void testCreationWithNulls() {
		final BaseMapperFactory factory = new BaseMapperFactory() {
			// nothing more to add
		};
		assertTrue(factory.addMapper(DateMapper.class));
		assertTrue(factory.addMapper(MockObjectMapper.class));

		final BaseMapper<?> mapper = factory.createWithNulls(Object.class,
				MilliSecond.instance());
		assertTrue(mapper instanceof MockObjectMapper);
	}

	/**
	 * Tests the creation using {@code null} values with a constructor which
	 * doesn't support {@code nulls}.
	 */
	@Test
	public void testCreationWithInvalidNulls() {
		final BaseMapperFactory factory = new BaseMapperFactory() {
			// nothing more to add
		};
		assertTrue(factory.addMapper(DateMapper.class));
		assertTrue(factory.addMapper(MockObjectMapper.class));

		final BaseMapper<?> mapper = factory.createWithNulls(Date.class,
				MilliSecond.instance());
		assertTrue(mapper instanceof MockObjectMapper);

	}

	/**
	 * Tests the creation with primitive parameters.
	 */
	@Test
	public void testCreationWithPrimitivesEndMapper() {
		final BaseMapperFactory factory = new BaseMapperFactory() {
			// nothing more to add
		};
		assertTrue(factory.addMapper(DateMapper.class));
		assertTrue(factory.addMapper(MockJustStartEndMapper.class));

		final BaseMapper<?> mapper = factory.createWithPrimitives(1, 5,
				MilliSecond.instance());
		assertTrue(mapper instanceof MockJustStartEndMapper);
	}

	/**
	 * Test the creation whereby several constructors are available.
	 */
	@Test
	public void testCreationWithSeveralConstructors() {
		final BaseMapperFactory factory = new BaseMapperFactory() {
			// nothing more to add
		};
		assertTrue(factory.addMapper(DateMapper.class));
		assertTrue(factory.addMapper(MockSeveralConstructorsMapper.class));

		final BaseMapper<?> mapper = factory.createWithNulls(Object.class,
				MilliSecond.instance());
		assertTrue(mapper instanceof MockSeveralConstructorsMapper);
	}

	/**
	 * Tests the creation whereby no mapper can be found.
	 */
	@Test
	public void testCreationWithInvalidMapper() {
		final BaseMapperFactory factory = new BaseMapperFactory() {
			// nothing more to add
		};

		assertTrue(factory.addMapper(MockSeveralConstructorsMapper.class));
		assertNotNull(factory.createWithObjects(new Date(), new Date(),
				MilliSecond.instance()));
	}

	/**
	 * Tests the creation with the selection of a super-type mapper.
	 */
	@Test
	public void testCreationWithSuperType() {
		final BaseMapperFactory factory = new BaseMapperFactory() {
			// nothing more to add
		};

		assertTrue(factory.addMapper(MockSeveralConstructorsMapper.class));
		assertTrue(factory.addMapper(DateMapper.class));

		assertTrue(factory.createWithObjects(new java.sql.Date(1000),
				new java.sql.Date(1000), MilliSecond.instance()) instanceof DateMapper);
	}

	/**
	 * Tests the selection of a constructor with an exception and a less good
	 * one to be choosen afterwards.
	 */
	@Test
	public void testCreationWithException() {
		final BaseMapperFactory factory = new BaseMapperFactory() {
			// nothing more to add
		};

		assertTrue(factory.addMapper(MockWithParametersMapper.class));
		assertTrue(factory.addMapper(MockSeveralConstructorsMapper.class));

		// get a mapper
		final BaseMapper<?> mapper = factory.createWithObjects(new Date(),
				new Date(), MilliSecond.instance(), new Date(), new Date());
		assertTrue("Found '" + mapper + "'",
				mapper instanceof MockSeveralConstructorsMapper);
	}
}
