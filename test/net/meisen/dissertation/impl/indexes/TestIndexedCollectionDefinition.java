package net.meisen.dissertation.impl.indexes;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

import java.math.BigDecimal;

import net.meisen.dissertation.impl.indexes.mock.ComplexIndexedCollection;
import net.meisen.dissertation.impl.indexes.mock.SimpleIndexedCollection;
import net.meisen.dissertation.model.indexes.BaseIndexedCollection;
import net.meisen.dissertation.model.indexes.IndexKeyDefinition;
import net.meisen.dissertation.model.indexes.IndexedCollectionDefinition;

import org.junit.Rule;
import org.junit.Test;
import org.junit.matchers.JUnitMatchers;
import org.junit.rules.ExpectedException;

/**
 * Tests the implementation of a {@code IndexedCollectionDefinition}.
 * 
 * @author pmeisen
 * 
 */
public class TestIndexedCollectionDefinition {

	/**
	 * Rule to evaluate exceptions
	 */
	@Rule
	public ExpectedException thrown = ExpectedException.none();

	/**
	 * Tests the creation of a {@code SimpleIndexedCollection} using a
	 * {@code IndexedCollectionDefinition}. This test includes the auto-adding
	 * of the {@link IndexedCollectionDefinition#INDEXKEYDEFINITION_PLACEHOLDER}
	 * and the correct usage.
	 */
	@Test
	public void testSimpleCreation() {
		final IndexKeyDefinition keyDef = new IndexKeyDefinition(
				TestIndexedCollectionDefinition.class);
		IndexedCollectionDefinition def;

		def = new IndexedCollectionDefinition(SimpleIndexedCollection.class);
		assertTrue(def.create(keyDef) instanceof SimpleIndexedCollection);
		assertEquals(keyDef,
				((BaseIndexedCollection) def.create(keyDef)).getKeyDefinition());

		def = new IndexedCollectionDefinition(SimpleIndexedCollection.class,
				IndexedCollectionDefinition.INDEXKEYDEFINITION_PLACEHOLDER);
		assertTrue(def.create(keyDef) instanceof SimpleIndexedCollection);
		assertEquals(keyDef,
				((BaseIndexedCollection) def.create(keyDef)).getKeyDefinition());
	}

	/**
	 * Tests the creation of a {@code SimpleIndexedCollection} using a
	 * {@code IndexedCollectionDefinition}.This test includes the correct
	 * constructor selection as long as super-type matching is possible.
	 */
	@Test
	public void testComplexCreation() {
		final IndexKeyDefinition keyDef = new IndexKeyDefinition(
				TestIndexedCollectionDefinition.class);
		IndexedCollectionDefinition def;

		ComplexIndexedCollection subject;

		def = new IndexedCollectionDefinition(ComplexIndexedCollection.class);
		subject = def.create(keyDef);
		assertTrue(subject instanceof ComplexIndexedCollection);
		assertEquals(1, subject.getParameter().size());
		assertEquals(keyDef, subject.getParameter().get(0));
		assertEquals(keyDef, subject.getKeyDefinition());

		def = new IndexedCollectionDefinition(ComplexIndexedCollection.class,
				IndexedCollectionDefinition.INDEXKEYDEFINITION_PLACEHOLDER);
		subject = def.create(keyDef);
		assertTrue(subject instanceof ComplexIndexedCollection);
		assertEquals(1, subject.getParameter().size());
		assertEquals(keyDef, subject.getParameter().get(0));
		assertEquals(keyDef, subject.getKeyDefinition());

		def = new IndexedCollectionDefinition(ComplexIndexedCollection.class,
				new Integer(5),
				IndexedCollectionDefinition.INDEXKEYDEFINITION_PLACEHOLDER,
				"WHATEVER", null);
		subject = def.create(keyDef);
		assertTrue(subject instanceof ComplexIndexedCollection);
		assertEquals(4, subject.getParameter().size());
		assertEquals(5, subject.getParameter().get(0));
		assertEquals(keyDef, subject.getParameter().get(1));
		assertEquals("WHATEVER", subject.getParameter().get(2));
		assertEquals(null, subject.getParameter().get(3));
		assertEquals(keyDef, subject.getKeyDefinition());

		def = new IndexedCollectionDefinition(ComplexIndexedCollection.class,
				IndexedCollectionDefinition.INDEXKEYDEFINITION_PLACEHOLDER,
				"WHATEVER");
		subject = def.create(keyDef);
		assertTrue(subject instanceof ComplexIndexedCollection);
		assertEquals(2, subject.getParameter().size());
		assertEquals(keyDef, subject.getParameter().get(0));
		assertEquals("WHATEVERMOD", subject.getParameter().get(1));
		assertEquals(keyDef, subject.getKeyDefinition());

		def = new IndexedCollectionDefinition(ComplexIndexedCollection.class,
				"WHATEVER");
		subject = def.create(keyDef);
		assertTrue(subject instanceof ComplexIndexedCollection);
		assertEquals(2, subject.getParameter().size());
		assertEquals(keyDef, subject.getParameter().get(0));
		assertEquals("WHATEVERMOD", subject.getParameter().get(1));
		assertEquals(keyDef, subject.getKeyDefinition());

		def = new IndexedCollectionDefinition(ComplexIndexedCollection.class,
				"WHATEVER",
				IndexedCollectionDefinition.INDEXKEYDEFINITION_PLACEHOLDER);
		subject = def.create(keyDef);
		assertTrue(subject instanceof ComplexIndexedCollection);
		assertEquals(2, subject.getParameter().size());
		assertEquals("WHATEVER", subject.getParameter().get(0));
		assertEquals(keyDef, subject.getParameter().get(1));
		assertEquals(keyDef, subject.getKeyDefinition());
	}

	/**
	 * Tests the creation of a {@code SimpleIndexedCollection} using a
	 * {@code IndexedCollectionDefinition}.This test includes the correct
	 * constructor selection by specifying the constructor to be used.
	 */
	@Test
	public void testForcedType() {
		final IndexKeyDefinition keyDef = new IndexKeyDefinition(
				TestIndexedCollectionDefinition.class);
		IndexedCollectionDefinition def;

		ComplexIndexedCollection subject;

		def = new IndexedCollectionDefinition(
				ComplexIndexedCollection.class,
				new Class[] { Integer.class, IndexKeyDefinition.class,
						Number.class },
				new Object[] {
						0,
						IndexedCollectionDefinition.INDEXKEYDEFINITION_PLACEHOLDER,
						new BigDecimal(5) });
		subject = def.create(keyDef);
		assertTrue(subject instanceof ComplexIndexedCollection);
		assertEquals(2, subject.getParameter().size());
		assertEquals(keyDef, subject.getParameter().get(0));
		assertEquals(Number.class, subject.getParameter().get(1));
		assertEquals(keyDef, subject.getKeyDefinition());

		def = new IndexedCollectionDefinition(
				ComplexIndexedCollection.class,
				new Class[] { Integer.class, IndexKeyDefinition.class,
						Integer.class },
				new Object[] {
						0,
						IndexedCollectionDefinition.INDEXKEYDEFINITION_PLACEHOLDER,
						new Integer(5) });
		subject = def.create(keyDef);
		assertTrue(subject instanceof ComplexIndexedCollection);
		assertEquals(2, subject.getParameter().size());
		assertEquals(keyDef, subject.getParameter().get(0));
		assertEquals(Integer.class, subject.getParameter().get(1));
		assertEquals(keyDef, subject.getKeyDefinition());

		// instead of a IndexKeyDefinition.class, null must be possible
		def = new IndexedCollectionDefinition(
				ComplexIndexedCollection.class,
				new Class[] { Integer.class, null, Integer.class },
				new Object[] {
						0,
						IndexedCollectionDefinition.INDEXKEYDEFINITION_PLACEHOLDER,
						new Integer(5) });
		subject = def.create(keyDef);
		assertTrue(subject instanceof ComplexIndexedCollection);
		assertEquals(2, subject.getParameter().size());
		assertEquals(keyDef, subject.getParameter().get(0));
		assertEquals(Integer.class, subject.getParameter().get(1));
		assertEquals(keyDef, subject.getKeyDefinition());
	}

	/**
	 * Tests the exception which should be found when {@code null} is passed as
	 * type.
	 */
	@Test
	public void testExceptionWhenNoType() {
		thrown.expect(NullPointerException.class);
		thrown.expectMessage(JUnitMatchers
				.containsString("type must be specified"));

		new IndexedCollectionDefinition(null);
	}

	/**
	 * Tests the exception which should be thrown when to many types are defined
	 * (in comparison to arguments).
	 */
	@Test
	public void testExceptionWhenToManyTypesAreDefined() {
		thrown.expect(IllegalArgumentException.class);
		thrown.expectMessage(JUnitMatchers
				.containsString("cannot be more types ('3') specified than argumentes ('2')"));

		new IndexedCollectionDefinition(SimpleIndexedCollection.class,
				new Class<?>[] { IndexKeyDefinition.class, String.class,
						Integer.class }, new Object[] { null });
	}

	/**
	 * Tests the exception thrown when an invalid type is defined (i.e. a type
	 * is defined at the placeholder-position).
	 */
	@Test
	public void testExceptionWhenInvalidTypesAreDefined() {
		thrown.expect(IllegalArgumentException.class);
		thrown.expectMessage(JUnitMatchers
				.containsString("type at position '1' must be '"
						+ IndexKeyDefinition.class.getName()
						+ "' or 'null' but is '" + String.class.getName() + "'"));

		new IndexedCollectionDefinition(
				SimpleIndexedCollection.class,
				new Class<?>[] { Integer.class, String.class, Integer.class },
				new Object[] {
						null,
						IndexedCollectionDefinition.INDEXKEYDEFINITION_PLACEHOLDER,
						null });
	}

	/**
	 * Tests the exception thrown when no constructor can be found.
	 */
	@Test
	public void testExceptionWhenNoConstructorCanBeFound() {
		thrown.expect(IllegalArgumentException.class);
		thrown.expectMessage(JUnitMatchers
				.containsString("Cannot find any valid constructor for the definition"));

		new IndexedCollectionDefinition(
				SimpleIndexedCollection.class,
				new Object[] {
						null,
						IndexedCollectionDefinition.INDEXKEYDEFINITION_PLACEHOLDER,
						null });
	}
}
