package net.meisen.dissertation.impl.parser.query.select.logical;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import java.util.Iterator;

import net.meisen.dissertation.config.TestConfig;
import net.meisen.dissertation.config.xslt.DefaultValues;
import net.meisen.dissertation.help.ModuleBasedTest;
import net.meisen.dissertation.impl.parser.query.select.SelectGroup;
import net.meisen.dissertation.model.data.MetaDataModel;
import net.meisen.dissertation.model.data.TidaModel;
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
		assertEquals(0, expr.getDescriptors().size());
	}

	/**
	 * Tests the setting and adding of descriptors.
	 */
	@Test
	public void testSetAndAddOfDescriptors() {
		final GroupExpression expr = new GroupExpression();
		Iterator<String> it;

		// add some descriptors
		assertTrue(expr.addDescriptors("one", "two", "three"));
		assertEquals(3, expr.getDescriptors().size());

		it = expr.getDescriptors().iterator();
		assertEquals("one", it.next());
		assertEquals("two", it.next());
		assertEquals("three", it.next());

		// add an already existing descriptor
		assertFalse(expr.addDescriptor("two"));
		assertEquals(3, expr.getDescriptors().size());

		it = expr.getDescriptors().iterator();
		assertEquals("one", it.next());
		assertEquals("two", it.next());
		assertEquals("three", it.next());

		// add several new, some already existing descriptors
		assertFalse(expr.addDescriptors("one", "four", "three"));
		assertEquals(4, expr.getDescriptors().size());

		it = expr.getDescriptors().iterator();
		assertEquals("one", it.next());
		assertEquals("two", it.next());
		assertEquals("three", it.next());
		assertEquals("four", it.next());

		// test the setting of a descriptor
		expr.setDescriptors("two");
		assertEquals(1, expr.getDescriptors().size());
		it = expr.getDescriptors().iterator();
		assertEquals("two", it.next());
	}

	/**
	 * Tests the implementation of
	 * {@code GroupExpression#addExclusion(String...)}.
	 */
	@Test
	public void testExclusions() {
		GroupExclusion excl;

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
		expr.setDescriptors("DESC1", "DESC2", "DESC3");
		expr.addExclusion("VAL1");
		assertTrue(expr.isValid());
		expr.removeExclusions();

		// add a valid exclusion
		expr.setDescriptors("DESC1", "DESC2", "DESC3");
		expr.addExclusion("VAL1", "VAL2", "VAL3");
		assertTrue(expr.isValid());
		expr.removeExclusions();

		// add a valid exclusion
		expr.setDescriptors("DESC1", "DESC2", "DESC3");
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
		expr.setDescriptors("DESC1", "DESC2", "DESC3");
		expr.addExclusion("VAL1", "VAL2", "VAL3");

		assertEquals(-1, expr.getPosition(null));
		assertEquals(-1, expr.getPosition("anyvalue"));

		assertEquals(0, expr.getPosition("DESC1"));
		assertEquals(1, expr.getPosition("DESC2"));
		assertEquals(2, expr.getPosition("DESC3"));
	}

	/**
	 * Tests the implementation of {@link GroupExpression#excludes(SelectGroup)}
	 * .
	 */
	@Test
	public void testExcludes() {
		GroupExpression expr;
		SelectGroup group;

		// initialize the module
		setModulesHolder("/net/meisen/dissertation/impl/parser/query/select/evaluator/testPersonModel.xml");
		final TidaModel model = modulesHolder
				.getModule(DefaultValues.TIDAMODEL_ID);
		model.initialize();
		model.loadData();

		// get the metaData
		final MetaDataModel metaData = model.getMetaDataModel();

		// test the appending and checking
		group = new SelectGroup();
		expr = new GroupExpression("PERSON", "LOCATION", "SCREAMS");
		expr.addExclusion("P*", "A*");

		// add just one value, which fits but isn't excluded yet
		group.append(metaData.getDescriptorByValue("PERSON", "Philipp"));
		assertFalse(expr.excludes(group));

		// append another value which achieves a fit
		group.append(metaData.getDescriptorByValue("LOCATION", "Aachen"));
		assertTrue(expr.excludes(group));

		// test a fast fit - e.g. we don't need to scan the whole group
		group = new SelectGroup();
		expr = new GroupExpression("PERSON", "LOCATION", "SCREAMS");
		expr.addExclusion("P*");

		group.append(metaData.getDescriptorByValue("PERSON", "Philipp"));
		assertTrue(expr.excludes(group));
		group.append(metaData.getDescriptorByValue("LOCATION", "Aachen"));
		assertTrue(expr.excludes(group));
	}
}
