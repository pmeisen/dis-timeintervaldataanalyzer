package net.meisen.dissertation.model.indexes;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import java.util.UUID;

import net.meisen.dissertation.impl.descriptors.IntegerDescriptor;
import net.meisen.dissertation.impl.idfactories.IntegerIdsFactory;
import net.meisen.dissertation.impl.indexes.mock.ConcreteValueCreator;
import net.meisen.dissertation.impl.indexes.mock.GenericValueCreator;
import net.meisen.dissertation.impl.indexes.mock.OverrideValueCreator;
import net.meisen.dissertation.model.descriptors.Descriptor;
import net.meisen.dissertation.model.descriptors.DescriptorModel;
import net.meisen.dissertation.model.indexes.IndexKeyDefinition;
import net.meisen.dissertation.model.indexes.keys.WrappedObjectIndexKey;
import net.meisen.dissertation.model.indexes.mock.ToIntIdResolver;

import org.junit.Rule;
import org.junit.Test;
import org.junit.matchers.JUnitMatchers;
import org.junit.rules.ExpectedException;

/**
 * Tests the implementation of a {@code IndexKey}.
 * 
 * @author pmeisen
 * 
 */
public class TestIndexKeyDefinition {

	/**
	 * Rule to evaluate exceptions
	 */
	@Rule
	public ExpectedException thrown = ExpectedException.none();

	/**
	 * Tests the retrieval of types within the {@code IndexKeyDefinition}
	 * implementation.
	 */
	@Test
	public void testTypeRetrieval() {

		// check the getValue of an abstract
		final IndexKeyDefinition keyDescriptor = new IndexKeyDefinition(
				Descriptor.class, "getValue");
		assertEquals(1, keyDescriptor.getTypes().length);
		assertEquals(Object.class, keyDescriptor.getTypes()[0]);

		// test a concrete implementation
		final IndexKeyDefinition keyIntegerDescriptor = new IndexKeyDefinition(
				IntegerDescriptor.class, "getValue");
		assertEquals(1, keyIntegerDescriptor.getTypes().length);
		assertEquals(Integer.class, keyIntegerDescriptor.getTypes()[0]);
	}

	/**
	 * Tests the retrieval of values of a {@code IndexKeyDefinition}
	 * implementation.
	 */
	@Test
	public void testValueRetrieval() {
		final IndexKeyDefinition keyDescriptor = new IndexKeyDefinition(
				Descriptor.class, "getValue");
		final IndexKeyDefinition keyIntegerDescriptor = new IndexKeyDefinition(
				Descriptor.class, "getValue");

		// create a descriptor
		final DescriptorModel<Integer> model = new DescriptorModel<Integer>(
				"MYID", IntegerDescriptor.class, new IntegerIdsFactory());
		final IntegerDescriptor<Integer> descriptor = new IntegerDescriptor<Integer>(
				model, 1, 5000);

		// check the generic values
		final Object[] genericValues = keyDescriptor.getValues(descriptor);
		assertEquals(5000, genericValues[0]);

		// check the integer values
		final Object[] integerValues = keyIntegerDescriptor
				.getValues(descriptor);
		assertEquals(5000, integerValues[0]);
	}

	/**
	 * Tests some different types of inheritance (override, abstract) in
	 * conjunction with the {@code IndexKeyDefinition} implementation.
	 */
	@Test
	public void testSomeInheritance() {

		// a generic class
		final IndexKeyDefinition key1 = new IndexKeyDefinition(
				GenericValueCreator.class, "getValue1", "getValue2");
		assertEquals(Object.class, key1.getTypes()[0]);
		assertEquals(Object.class, key1.getTypes()[1]);

		// a concrete implementation
		final IndexKeyDefinition key2 = new IndexKeyDefinition(
				ConcreteValueCreator.class, "getValue1", "getValue2");
		assertEquals(Number.class, key2.getTypes()[0]);
		assertEquals(Object.class, key2.getTypes()[1]);

		// an implementation which overrides
		final IndexKeyDefinition key3 = new IndexKeyDefinition(
				OverrideValueCreator.class, "getValue1", "getValue2");
		assertEquals(Number.class, key3.getTypes()[0]);
		assertEquals(String.class, key3.getTypes()[1]);

		// check the results of some calls
		final ConcreteValueCreator concrete = new ConcreteValueCreator();
		final OverrideValueCreator override = new OverrideValueCreator();

		// check the retrieved values by key1
		assertEquals(5, key1.getValues(concrete)[0]);
		assertEquals("MyValue", key1.getValues(concrete)[1]);
		assertEquals(5, key1.getValues(override)[0]);
		assertEquals("IDONTHAVEAVALUE", key1.getValues(override)[1]);

		// check the once of key2
		assertEquals(5, key2.getValues(concrete)[0]);
		assertEquals("MyValue", key2.getValues(concrete)[1]);
		assertEquals(5, key2.getValues(override)[0]);
		assertEquals("IDONTHAVEAVALUE", key2.getValues(override)[1]);

		// check the once of key3
		assertEquals(5, key3.getValues(override)[0]);
		assertEquals("IDONTHAVEAVALUE", key3.getValues(override)[1]);
	}

	/**
	 * Tests the primitive type mapper
	 */
	@Test
	public void testPrimitiveTypeMapper() {
		final IndexKeyDefinition key = new IndexKeyDefinition(Integer.class,
				"intValue");

		assertEquals(Integer.class, key.primitiveTypeMapper(int.class));
		assertEquals(Double.class, key.primitiveTypeMapper(double.class));
		assertEquals(String.class, key.primitiveTypeMapper(String.class));
		assertEquals(TestIndexKeyDefinition.class,
				key.primitiveTypeMapper(TestIndexKeyDefinition.class));
	}

	/**
	 * Tests the usage of primitive types
	 */
	@Test
	public void testPrimitiveTypes() {
		final IndexKeyDefinition keyInteger = new IndexKeyDefinition(
				Integer.class, "intValue");
		assertEquals(Integer.class, keyInteger.getTypes()[0]);
		assertEquals(5, keyInteger.getValues(new Integer(5))[0]);
	}

	/**
	 * Tests the implementation of
	 * {@link IndexKeyDefinition#isSingleTypedKey(Class)}.
	 */
	@Test
	public void testIsSingleTypedKey() {
		final IndexKeyDefinition keyIntegerMethod = new IndexKeyDefinition(
				Integer.class, "intValue");

		assertTrue(keyIntegerMethod.isSingleTypedKey(Integer.class));
		assertFalse(keyIntegerMethod.isSingleTypedKey(Long.class));

		final IndexKeyDefinition keyInteger = new IndexKeyDefinition(
				Integer.class);
		assertTrue(keyInteger.isSingleTypedKey(Integer.class));
		assertTrue(keyInteger.isSingleTypedKey(int.class));
		assertFalse(keyInteger.isSingleTypedKey(Long.class));

		final IndexKeyDefinition keyLong = new IndexKeyDefinition(Long.class);
		assertTrue(keyLong.isSingleTypedKey(Long.class));
		assertFalse(keyLong.isSingleTypedKey(Integer.class));
		assertFalse(keyLong.isSingleTypedKey(String.class));
		assertFalse(keyLong.isSingleTypedKey(int.class));
		assertTrue(keyLong.isSingleTypedKey(long.class));
		assertFalse(keyLong.isSingleTypedKey(Number.class));

		final IndexKeyDefinition keyIdResolver = new IndexKeyDefinition(
				UUID.class, new ToIntIdResolver(UUID.class));
		assertTrue(keyIdResolver.isSingleTypedKey(Integer.class));
		assertFalse(keyIdResolver.isSingleTypedKey(Long.class));
		assertFalse(keyIdResolver.isSingleTypedKey(Number.class));
	}

	/**
	 * Test the implementation of
	 * {@link IndexKeyDefinition#getValue(Object, Class)}.
	 */
	@Test
	public void testGetValue() {
		final IndexKeyDefinition key1 = new IndexKeyDefinition(Integer.class,
				"intValue");
		assertEquals(key1.getValue(new Integer(5), Integer.class), new Integer(
				5));

		final IndexKeyDefinition key2 = new IndexKeyDefinition(Integer.class);
		assertEquals(key2.getValue(new Integer(15), Integer.class),
				new Integer(15));

		final IndexKeyDefinition key3 = new IndexKeyDefinition(String.class,
				"toString");
		assertEquals(key3.getValue("Hallo", String.class), "Hallo");
	}

	/**
	 * Tests the implementation of
	 * {@link IndexKeyDefinition#generateKeyFromValues(Object...)}.
	 */
	@Test
	public void testGenerateKeyFromValues() {
		final IndexKeyDefinition key1 = new IndexKeyDefinition(Integer.class,
				"intValue");
		assertEquals(key1.getKey(new Integer(5)), key1.generateKeyFromValues(5));

		final IndexKeyDefinition key2 = new IndexKeyDefinition(UUID.class,
				"toString");
		final UUID o = UUID.randomUUID();
		assertEquals(key2.getKey(o), key2.generateKeyFromValues(o.toString()));

		final IndexKeyDefinition key3 = new IndexKeyDefinition(UUID.class,
				"toString", "getClass");
		assertEquals(key3.getKey(o),
				key3.generateKeyFromValues(o.toString(), UUID.class));
	}

	/**
	 * Tests the implementation of
	 * {@link IndexKeyDefinition#generateTypedKeyFromValue(Object, Class)}.
	 */
	@Test
	public void testGenerateTypedKeyFromValues() {
		final IndexKeyDefinition key1 = new IndexKeyDefinition(Integer.class,
				"intValue");
		assertEquals(key1.generateTypedKeyFromValue(5, Integer.class),
				new Integer(5));

		final IndexKeyDefinition key2 = new IndexKeyDefinition(Integer.class,
				"getClass");
		assertEquals(key2.generateTypedKeyFromValue(String.class, Class.class),
				String.class);

		final IndexKeyDefinition key3 = new IndexKeyDefinition(String.class);
		assertEquals(key3.generateTypedKeyFromValue("hi", String.class), "hi");

		final IndexKeyDefinition key4 = new IndexKeyDefinition(Number.class);
		assertEquals(new Long(5),
				key4.generateTypedKeyFromValue(new Long(5), Long.class));
	}

	/**
	 * Tests the usage of an {@code IdResolver}.
	 */
	@Test
	public void testIdResolver() {
		final ToIntIdResolver idResolver = new ToIntIdResolver(UUID.class);

		// test a KeyDefinition with a resolver
		final IndexKeyDefinition keyDef = new IndexKeyDefinition(UUID.class,
				idResolver);
		for (int i = 0; i < 100; i++) {
			final UUID uuid = UUID.randomUUID();
			final Object key = keyDef.getKey(uuid);
			assertTrue(key instanceof WrappedObjectIndexKey);
			assertEquals(i, ((WrappedObjectIndexKey) key).getWrappedObject());
		}
		assertEquals(100, idResolver.reset());

		// check a typed call
		for (int i = 0; i < 100; i++) {
			final UUID uuid = UUID.randomUUID();
			final int key = keyDef.getIntKey(uuid);
			assertEquals(i, key);
		}
	}

	/**
	 * Tests the implementation of {@code IndexKeyDefinition#matches(Object...)}
	 * .
	 */
	@Test
	public void testMatches() {
		IndexKeyDefinition keyDef;

		// KeyDefinition with a single Object
		keyDef = new IndexKeyDefinition(UUID.class);
		assertTrue(keyDef.matches((Object) null));
		assertTrue(keyDef.matches(UUID.randomUUID()));
		assertFalse(keyDef.matches("HelloWorld"));
		assertFalse(keyDef.matches(5));

		// KeyDefinition with methods
		keyDef = new IndexKeyDefinition(UUID.class, "toString", "getClass");
		assertTrue(keyDef.matches((Object) null, (Object) null));
		assertTrue(keyDef.matches("HelloWorld", TestIndexKeyDefinition.class));
		assertFalse(keyDef.matches((Object) null, 5));
		assertFalse(keyDef.matches(5));
		assertFalse(keyDef.matches(5, 10, 2));
		assertFalse(keyDef.matches("HelloWorld", UUID.class, 4));

		// KeyDefinition with idResolver
		final ToIntIdResolver idResolver = new ToIntIdResolver(UUID.class);
		keyDef = new IndexKeyDefinition(UUID.class, idResolver);
		assertTrue(keyDef.matches((Object) null));
		assertTrue(keyDef.matches(5));
		assertTrue(keyDef.matches(new Integer(5)));
		assertFalse(keyDef.matches(5l));
		assertFalse(keyDef.matches((Object) null, (Object) null));
	}

	/**
	 * Tests the invalid construction of a {@code IndexKeyDefinition} using a
	 * {@code null} method.
	 */
	@Test
	public void testInvalidMethodNullConstrcution2() {
		thrown.expect(NullPointerException.class);
		thrown.expectMessage(JUnitMatchers
				.containsString("getterMethods cannot be null"));

		new IndexKeyDefinition(GenericValueCreator.class, (String[]) null);
	}

	/**
	 * Tests the invalid construction of a {@code IndexKeyDefinition} using
	 * null-methods.
	 */
	@Test
	public void testInvalidMethodWithNullConstrcution() {
		thrown.expect(IllegalArgumentException.class);
		thrown.expectMessage(JUnitMatchers
				.containsString("method 'null' cannot be accessed or found"));

		new IndexKeyDefinition(GenericValueCreator.class, null, null);
	}

	/**
	 * Tests the invalid construction of a {@code IndexKeyDefinition} using an
	 * invalid method.
	 */
	@Test
	public void testInvalidMethodNameConstrcution() {
		thrown.expect(IllegalArgumentException.class);
		thrown.expectMessage(JUnitMatchers
				.containsString("method 'getValue3' cannot be accessed or found"));

		new IndexKeyDefinition(GenericValueCreator.class, "getValue3");
	}

	/**
	 * Tests the retrieval of values from a {@code null} object.
	 */
	@Test
	public void testInvalidNullValueRetrieval() {
		thrown.expect(NullPointerException.class);
		thrown.expectMessage(JUnitMatchers
				.containsString("object cannot be null"));

		final IndexKeyDefinition key = new IndexKeyDefinition(
				OverrideValueCreator.class, "getValue1", "getValue2");
		key.getValues(null);
	}

	/**
	 * Tests the retrieval of a value from an invalid super class, i.e. the
	 * {@code IndexKeyDefinition} is based on a class, whereby an object of a
	 * super-class is passed.
	 */
	@Test
	public void testInvalidValueRetrieval() {
		thrown.expect(IllegalArgumentException.class);
		thrown.expectMessage(JUnitMatchers.containsString("passed object '"
				+ ConcreteValueCreator.class.getName()
				+ "' cannot be assigned by index-class '"
				+ OverrideValueCreator.class.getName() + "'"));

		// try to retrieve a value from a super class
		final ConcreteValueCreator concrete = new ConcreteValueCreator();
		final IndexKeyDefinition key = new IndexKeyDefinition(
				OverrideValueCreator.class, "getValue1", "getValue2");
		key.getValues(concrete);
	}

	/**
	 * Tests the exception to be thrown, when a void method is used within the
	 * definition.
	 */
	@Test
	public void testInvalidVoidMethodConstrcution() {
		thrown.expect(IllegalArgumentException.class);
		thrown.expectMessage(JUnitMatchers
				.containsString("method 'testInvalidVoidMethodConstrcution' doesn't return any value"));

		new IndexKeyDefinition(TestIndexKeyDefinition.class,
				"testInvalidVoidMethodConstrcution");
	}

	/**
	 * Test the implementation of
	 * {@link IndexKeyDefinition#getValue(Object, Class)} using a composite key
	 * with an invalid expectation.
	 */
	@Test
	public void testInvalidGetValueFromComposite() {
		thrown.expect(IllegalStateException.class);
		thrown.expectMessage(JUnitMatchers
				.containsString("within an IndexKeyDefinition with multiple keys"));

		final IndexKeyDefinition key = new IndexKeyDefinition(Integer.class,
				"intValue", "getClass");
		key.getValue(new Integer(5), Integer.class);
	}

	/**
	 * Test the implementation of
	 * {@link IndexKeyDefinition#getValue(Object, Class)} using a single-value
	 * key with an invalid expectation.
	 */
	@Test
	public void testInvalidGetValueExpectationWithMethod() {
		thrown.expect(IllegalArgumentException.class);
		thrown.expectMessage(JUnitMatchers
				.containsString("which is assignable from '"
						+ Long.class.getName() + "' but got '5' ('"
						+ Integer.class.getName() + "')"));

		final IndexKeyDefinition key = new IndexKeyDefinition(Integer.class,
				"intValue");
		key.getValue(new Integer(5), Long.class);
	}

	/**
	 * Test the implementation of
	 * {@link IndexKeyDefinition#getValue(Object, Class)} using an object key
	 * with an invalid expectation.
	 */
	@Test
	public void testInvalidGetValueExpectationWithObject() {
		thrown.expect(IllegalArgumentException.class);
		thrown.expectMessage(JUnitMatchers
				.containsString("Expected an object which is assignable from '"
						+ Long.class.getName() + "' but got '5' ('"
						+ Integer.class.getName() + "')"));

		final IndexKeyDefinition key = new IndexKeyDefinition(Number.class);
		key.getValue(new Integer(5), Long.class);
	}

	/**
	 * Tests the implementation of
	 * {@link IndexKeyDefinition#getValue(Object, Class)} using an key and an
	 * expectation which is not compatible.
	 */
	@Test
	public void testInvalidGetValueExceptionObjectClass() {
		thrown.expect(IllegalArgumentException.class);
		thrown.expectMessage(JUnitMatchers.containsString("expected class '"
				+ Long.class.getName()
				+ "' is not compatible to the key-class '"
				+ Integer.class.getName() + "'"));

		final IndexKeyDefinition key = new IndexKeyDefinition(Integer.class);
		key.getValue(new Integer(5), Long.class);
	}

	/**
	 * Tests exception thrown with
	 * {@link IndexKeyDefinition#generateTypedKeyFromValue(Object, Class)} using
	 * multiple keys.
	 */
	@Test
	public void testInvalidGenerateTypedKeyFromValueUsingMultiple() {
		thrown.expect(IllegalStateException.class);
		thrown.expectMessage(JUnitMatchers
				.containsString("generate a key from a single value, within an IndexKeyDefinition with multiple keys"));

		final IndexKeyDefinition key = new IndexKeyDefinition(Integer.class,
				"intValue", "getClass");
		key.generateTypedKeyFromValue(5, Integer.class);
	}

	/**
	 * Tests exception thrown with
	 * {@link IndexKeyDefinition#generateTypedKeyFromValue(Object, Class)} using
	 * a wrong value.
	 */
	@Test
	public void testInvalidGenerateTypedKeyFromValueWrongValue() {
		thrown.expect(IllegalArgumentException.class);
		thrown.expectMessage(JUnitMatchers.containsString("object '" + 5
				+ "' ('" + Long.class.getName()
				+ "') is not of the expected type '" + Integer.class.getName()
				+ "'"));

		final IndexKeyDefinition key = new IndexKeyDefinition(String.class);
		key.generateTypedKeyFromValue(new Long(5), Integer.class);
	}

	/**
	 * Tests exception thrown with
	 * {@link IndexKeyDefinition#generateTypedKeyFromValue(Object, Class)} using
	 * a wrong object class.
	 */
	@Test
	public void testInvalidGenerateTypedKeyFromValueWrongObjectClass() {
		thrown.expect(IllegalArgumentException.class);
		thrown.expectMessage(JUnitMatchers.containsString("expected class '"
				+ Long.class.getName()
				+ "' is not compatible to the key-class '"
				+ String.class.getName() + "'"));

		final IndexKeyDefinition key = new IndexKeyDefinition(String.class);
		key.generateTypedKeyFromValue(new Long(5), Long.class);
	}

	/**
	 * Tests exception thrown with
	 * {@link IndexKeyDefinition#generateTypedKeyFromValue(Object, Class)} using
	 * a wrong method.
	 */
	@Test
	public void testInvalidGenerateTypedKeyFromValueWrongMethod() {
		thrown.expect(IllegalArgumentException.class);
		thrown.expectMessage(JUnitMatchers.containsString("expected '"
				+ Long.class.getName()
				+ "' isn't compatible to the key-class '"
				+ String.class.getName() + "'"));

		final IndexKeyDefinition key = new IndexKeyDefinition(Long.class,
				"toString");
		key.generateTypedKeyFromValue(new Long(5), Long.class);
	}
}
