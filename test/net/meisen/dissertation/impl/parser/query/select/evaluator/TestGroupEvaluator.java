package net.meisen.dissertation.impl.parser.query.select.evaluator;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;

import java.util.List;

import net.meisen.dissertation.config.TestConfig;
import net.meisen.dissertation.config.xslt.DefaultValues;
import net.meisen.dissertation.help.ModuleBasedTest;
import net.meisen.dissertation.impl.descriptors.GeneralDescriptor;
import net.meisen.dissertation.impl.descriptors.IntegerDescriptor;
import net.meisen.dissertation.impl.descriptors.ResourceDescriptor;
import net.meisen.dissertation.impl.parser.query.select.SelectGroup;
import net.meisen.dissertation.impl.parser.query.select.evaluator.GroupEvaluator.GroupValues;
import net.meisen.dissertation.impl.parser.query.select.logical.GroupExpression;
import net.meisen.dissertation.model.data.MetaDataModel;
import net.meisen.dissertation.model.data.TidaModel;
import net.meisen.dissertation.model.descriptors.Descriptor;
import net.meisen.general.genmisc.exceptions.ForwardedRuntimeException;
import net.meisen.general.sbconfigurator.runners.annotations.ContextClass;
import net.meisen.general.sbconfigurator.runners.annotations.ContextFile;

import org.hamcrest.Description;
import org.junit.Before;
import org.junit.Test;
import org.junit.internal.matchers.TypeSafeMatcher;

/**
 * Tests the implementation of a {@code GroupEvaluator}.
 * 
 * @author pmeisen
 * 
 */
@ContextClass(TestConfig.class)
@ContextFile("test-sbconfigurator-core.xml")
public class TestGroupEvaluator extends ModuleBasedTest {

	private TidaModel model;

	/**
	 * Initialize the model used for this test.
	 */
	@Before
	public void init() {

		// initialize the module
		setModulesHolder("/net/meisen/dissertation/impl/parser/query/select/evaluator/testPersonModel.xml");
		model = modulesHolder.getModule(DefaultValues.TIDAMODEL_ID);
		model.initialize();
		model.loadData();
	}

	/**
	 * Tests the exception to be thrown by the
	 * {@link GroupEvaluator#getDescriptorValues(String, GroupExpression)}
	 * implementation, when an invalid model is used.
	 */
	@Test
	public void testGetDescriptorValuesWithInvalidModelDescriptor() {
		thrown.expect(ForwardedRuntimeException.class);
		thrown.expect(new TypeSafeMatcher<ForwardedRuntimeException>() {
			private final String expected = "Number: '1000'";

			@Override
			public void describeTo(final Description description) {
				description.appendText(expected);
			}

			@Override
			public boolean matchesSafely(final ForwardedRuntimeException item) {
				return item.toString().contains(expected);
			}
		});

		// create the evaluator
		final GroupEvaluator evaluator = new GroupEvaluator(model);

		// check a null descriptorId
		evaluator.getDescriptorValues(null, new GroupExpression());
	}

	/**
	 * Tests the exception to be thrown by the
	 * {@link GroupEvaluator#getDescriptorValues(String, GroupExpression)}
	 * implementation, when a valid model, but invalid for the group, is used.
	 */
	@Test
	public void testGetDescriptorValuesWithInvalidGroupDescriptor() {
		thrown.expect(ForwardedRuntimeException.class);
		thrown.expect(new TypeSafeMatcher<ForwardedRuntimeException>() {
			private final String expected = "Number: '1001'";

			@Override
			public void describeTo(final Description description) {
				description.appendText(expected);
			}

			@Override
			public boolean matchesSafely(final ForwardedRuntimeException item) {
				return item.toString().contains(expected);
			}
		});

		// create the evaluator
		final GroupEvaluator evaluator = new GroupEvaluator(model);

		// check a null descriptorId
		final GroupExpression expression = new GroupExpression();
		expression.addDescriptor("IDONTEXIST");
		evaluator.getDescriptorValues("PERSON", expression);
	}

	/**
	 * Tests the
	 * {@link GroupEvaluator#getDescriptorValues(String, GroupExpression)}
	 * implementation.
	 */
	@Test
	public void testGetDescriptorValues() {
		GroupExpression group;
		GroupValues res;

		// create the evaluator
		final GroupEvaluator evaluator = new GroupEvaluator(model);

		// check a group with all values (i.e. no exclusion)
		group = new GroupExpression();
		group.addDescriptors("PERSON", "LOCATION");
		res = evaluator.getDescriptorValues("PERSON", group);
		assertEquals(4, res.size());
		assertEquals("PERSON", res.getDescriptorModelId());

		// test the location with the null-pointer
		res = evaluator.getDescriptorValues("LOCATION", group);
		assertEquals(3, res.size());
		assertEquals("LOCATION", res.getDescriptorModelId());

		// test the data based descriptor
		group = new GroupExpression();
		group.addDescriptors("SCREAMS");
		res = evaluator.getDescriptorValues("SCREAMS", group);
		assertEquals(3, res.size());
		assertEquals("SCREAMS", res.getDescriptorModelId());
	}

	/**
	 * Tests the generation of groups, i.e.
	 * {@link GroupEvaluator#generateGroups(GroupExpression)}.
	 */
	@Test
	public void testGenerateGroups() {
		final MetaDataModel m = model.getMetaDataModel();

		List<SelectGroup> res;
		GroupExpression group;

		// create the evaluator
		final GroupEvaluator evaluator = new GroupEvaluator(model);

		// generate the groups of an empty expression
		group = new GroupExpression();
		res = evaluator.generateGroups(group);
		assertNull(res);

		// generate a group for one descriptor
		group = new GroupExpression("PERSON");
		res = evaluator.generateGroups(group);
		assertEquals(4, res.size());
		assertContains(res, m.getDescriptorByValue("PERSON", "Philipp"), 0);
		assertContains(res, m.getDescriptorByValue("PERSON", "Debbie"), 0);
		assertContains(res, m.getDescriptorByValue("PERSON", "Tobias"), 0);
		assertContains(res, m.getDescriptorByValue("PERSON", "*Edison*"), 0);

		// generate a group for two descriptors
		group = new GroupExpression("SCREAMS", "PERSON");
		res = evaluator.generateGroups(group);
		assertEquals(12, res.size());
		for (final SelectGroup g : res) {
			assertEquals(2, g.size());
			assertTrue(g.getDescriptor(0) instanceof IntegerDescriptor);
			assertTrue(g.getDescriptor(1) instanceof ResourceDescriptor);
		}

		// generate a group for three descriptors
		group = new GroupExpression("LOCATION", "SCREAMS", "PERSON");
		res = evaluator.generateGroups(group);
		assertEquals(36, res.size());
		for (final SelectGroup g : res) {
			assertEquals(3, g.size());

			/*
			 * the 0 position contains also a NullDescriptor as well as a
			 * GeneralDescriptor, therefore we don't test it here
			 */
			assertTrue(g.getDescriptor(1) instanceof IntegerDescriptor);
			assertTrue(g.getDescriptor(2) instanceof ResourceDescriptor);
		}

		// test the exclusion of values
		group = new GroupExpression("SCREAMS", "PERSON");
		group.addExclusion("*", "P*");
		res = evaluator.generateGroups(group);
		assertEquals(9, res.size());
		for (final SelectGroup g : res) {

			// the descriptor 'Philipp' should been excluded
			assertTrue(!g.getDescriptor(1).getUniqueString().equals("Philipp"));
		}

		// test the exclusion of values

		group = new GroupExpression("SCREAMS", "PERSON");
		group.addExclusion("*", "P*");
		group.addExclusion("12", "\\**");
		res = evaluator.generateGroups(group);
		assertEquals(8, res.size());
		for (final SelectGroup g : res) {

			// the descriptor 'Philipp' should been excluded
			assertTrue(!g.getDescriptor(1).getValue().equals("Philipp"));

			// the '*Edison*' with '12' screams should been excluded
			assertTrue(!g.getDescriptor(1).getValue().equals("*Edison*")
					|| !g.getDescriptor(0).getValue().equals(12));
		}

		// test a full exclusion
		group = new GroupExpression("SCREAMS", "PERSON");
		group.addExclusion("*");
		res = evaluator.generateGroups(group);
		assertEquals(0, res.size());
	}

	/**
	 * Tests an empty {@code GroupEvaluator} using an empty
	 * {@code GroupExpression}.
	 */
	@Test
	public void testEmptyEvaluateGroupExpression() {
		GroupExpression group;

		// create the evaluator
		final GroupEvaluator evaluator = new GroupEvaluator(model);
		group = new GroupExpression();
		evaluator.evaluateGroupExpression(group);
	}

	/**
	 * Tests a {@code GroupEvaluator} using a {@code GroupExpression} without
	 * any exclusion.
	 */
	@Test
	public void testEvaluateGroupExpressionWithoutExclusions() {
		GroupExpression group;

		// create the evaluator
		final GroupEvaluator evaluator = new GroupEvaluator(model);
		group = new GroupExpression();
		group.setDescriptors("PERSON", "LOCATION");
		assertTrue(group.isValid());

		evaluator.evaluateGroupExpression(group);
	}

	/**
	 * 
	 */
	@Test
	public void testEvaluateGroupExpressionWithExclusions() {
		GroupExpression group;

		// create the evaluator
		final GroupEvaluator evaluator = new GroupEvaluator(model);
		group = new GroupExpression();
		group.setDescriptors("PERSON", "LOCATION");
		assertTrue(group.isValid());

		evaluator.evaluateGroupExpression(group);
	}

	private void assertContains(final List<SelectGroup> groups,
			final Descriptor<String, ?, ?> desc, final int position) {
		for (final SelectGroup group : groups) {
			if (group.getDescriptor(position).equals(desc)) {
				return;
			}
		}

		assertFalse("Unable to find '" + desc + "' in '" + groups
				+ "' at position '" + position + "'.", false);
	}
}
