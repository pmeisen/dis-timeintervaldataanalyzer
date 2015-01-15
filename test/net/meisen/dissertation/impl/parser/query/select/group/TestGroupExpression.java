package net.meisen.dissertation.impl.parser.query.select.group;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import java.util.Iterator;

import net.meisen.dissertation.config.TestConfig;
import net.meisen.dissertation.help.ModuleBasedTest;
import net.meisen.dissertation.impl.parser.query.DimensionSelector;
import net.meisen.dissertation.impl.parser.query.select.group.GroupFilter;
import net.meisen.dissertation.impl.parser.query.select.group.GroupExpression;
import net.meisen.general.sbconfigurator.runners.annotations.ContextClass;
import net.meisen.general.sbconfigurator.runners.annotations.ContextFile;

import org.junit.Test;

/**
 * Tests the implementation of a {@code GroupExpression} used within a select
 * query.
 * 
 * @author pmeisen
 * 
 */
@ContextClass(TestConfig.class)
@ContextFile("test-sbconfigurator-core.xml")
public class TestGroupExpression extends ModuleBasedTest {

	/**
	 * Tests an empty group.
	 */
	@Test
	public void testEmptyDescriptors() {
		final GroupExpression expr = new GroupExpression();
		assertEquals(0, expr.getSelectors().size());
	}

	/**
	 * Tests the setting and adding of descriptors.
	 */
	@Test
	public void testSetAndAddOfDescriptors() {
		final GroupExpression expr = new GroupExpression();
		Iterator<Object> it;

		// add some descriptors
		assertTrue(expr.addSelectors("one", "two", "three"));
		assertEquals(3, expr.getSelectors().size());

		it = expr.getSelectors().iterator();
		assertEquals("one", it.next());
		assertEquals("two", it.next());
		assertEquals("three", it.next());

		// add an already existing descriptor
		assertFalse(expr.addSelector("two"));
		assertEquals(3, expr.getSelectors().size());

		it = expr.getSelectors().iterator();
		assertEquals("one", it.next());
		assertEquals("two", it.next());
		assertEquals("three", it.next());

		// add several new, some already existing descriptors
		assertFalse(expr.addSelectors("one", "four", "three"));
		assertEquals(4, expr.getSelectors().size());

		it = expr.getSelectors().iterator();
		assertEquals("one", it.next());
		assertEquals("two", it.next());
		assertEquals("three", it.next());
		assertEquals("four", it.next());

		// test the setting of a descriptor
		expr.setSelectors("two");
		assertEquals(1, expr.getSelectors().size());
		it = expr.getSelectors().iterator();
		assertEquals("two", it.next());
	}

	/**
	 * Tests the implementation of
	 * {@code GroupExpression#addExclusion(String...)}.
	 */
	@Test
	public void testExclusions() {
		GroupFilter excl;

		// check it empty
		final GroupExpression expr = new GroupExpression();
		assertEquals(0, expr.getExclusions().size());

		// check the adding of an exclusion
		expr.addExclusion("val1", null);
		assertEquals(1, expr.getExclusions().size());

		// check if everything is set
		excl = expr.getExclusions().get(0);
		assertEquals("val1", excl.getValues().get(0).getValue());
		assertEquals(null, excl.getValues().get(1).getValue());

		// add another one
		expr.addExclusion("val2", "location");
		excl = expr.getExclusions().get(1);
		assertEquals("val2", excl.getValues().get(0).getValue());
		assertEquals("location", excl.getValues().get(1).getValue());
	}

	/**
	 * Tests the validation of a {@code GroupExpression}.
	 */
	@Test
	public void testValidation() {
		final GroupExpression expr = new GroupExpression();

		// an empty expression should be valid
		assertTrue(expr.isValid());

		// add an exclusion which is to small (rest will be *)
		expr.setSelectors("DESC1", "DESC2", "DESC3");
		expr.addExclusion("VAL1");
		assertTrue(expr.isValid());
		expr.removeExclusions();

		// add a valid exclusion
		expr.setSelectors("DESC1", "DESC2", "DESC3", new DimensionSelector(
				"DIM", "HIER", "LEVEL"));
		expr.addExclusion("VAL1", "VAL2", "VAL3", "VAL4");
		assertTrue(expr.isValid());
		expr.removeExclusions();

		// add a valid exclusion
		expr.setSelectors("DESC1", "DESC2", "DESC3");
		expr.addExclusion("VAL1", null, "VAL3");
		assertTrue(expr.isValid());
		expr.removeExclusions();
	}

	/**
	 * Tests the implementation to determine the exclusion's position.
	 */
	@Test
	public void testGetExclusionPosition() {
		final GroupExpression expr = new GroupExpression();
		expr.setSelectors("DESC1", "DESC2", new DimensionSelector("DIM",
				"HIER", "LEVEL"), "DESC3");
		expr.addExclusion("VAL1", "VAL2", "VAL3");

		assertEquals(-1, expr.getPosition(null));
		assertEquals(-1, expr.getPosition("anyvalue"));
		assertEquals(-1, expr.getPosition(new DimensionSelector("DIM", "HIER",
				"LEVEL2")));

		assertEquals(0, expr.getPosition("DESC1"));
		assertEquals(1, expr.getPosition("DESC2"));
		assertEquals(2,
				expr.getPosition(new DimensionSelector("DIM", "HIER", "LEVEL")));
		assertEquals(3, expr.getPosition("DESC3"));
	}
}
