package net.meisen.dissertation.impl.parser.query.select.evaluator;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;

import java.util.Arrays;
import java.util.List;

import net.meisen.dissertation.config.TestConfig;
import net.meisen.dissertation.config.xslt.DefaultValues;
import net.meisen.dissertation.exceptions.GroupEvaluatorException;
import net.meisen.dissertation.help.ModuleBasedTest;
import net.meisen.dissertation.impl.descriptors.GeneralDescriptor;
import net.meisen.dissertation.impl.descriptors.IntegerDescriptor;
import net.meisen.dissertation.impl.parser.query.DimensionSelector;
import net.meisen.dissertation.impl.parser.query.select.evaluator.GroupEvaluator.Group;
import net.meisen.dissertation.impl.parser.query.select.evaluator.GroupEvaluator.GroupEntry;
import net.meisen.dissertation.impl.parser.query.select.group.GroupExpression;
import net.meisen.dissertation.model.data.MetaDataModel;
import net.meisen.dissertation.model.data.TidaModel;
import net.meisen.dissertation.model.descriptors.Descriptor;
import net.meisen.general.genmisc.exceptions.ForwardedRuntimeException;
import net.meisen.general.sbconfigurator.runners.annotations.ContextClass;
import net.meisen.general.sbconfigurator.runners.annotations.ContextFile;

import org.junit.After;
import org.junit.Before;
import org.junit.Test;

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
		model.bulkLoadDataFromDataModel();
	}

	/**
	 * Tests the implementation of {@link GroupEvaluator#excludes(Group, List)}
	 * .
	 */
	@Test
	public void testExcludes() {
		GroupExpression expr;
		Group group;
		Descriptor<?, ?, ?> desc;

		// create the evaluator
		final GroupEvaluator evaluator = new GroupEvaluator(model);

		// get the metaData
		final MetaDataModel metaData = model.getMetaDataModel();

		// test the appending and checking
		group = new Group();
		expr = new GroupExpression("PERSON", "LOCATION", "SCREAMS");
		expr.addExclusion("P*", "A*");

		// add just one value, which fits but isn't excluded yet
		desc = metaData.getDescriptorByValue("PERSON", "Philipp");
		group.append(new GroupEntry<Descriptor<?, ?, ?>>(
				desc.getUniqueString(), desc));
		assertFalse(evaluator.excludes(group, expr.getExclusions()));

		// append another value which achieves a fit
		desc = metaData.getDescriptorByValue("LOCATION", "Aachen");
		group.append(new GroupEntry<Descriptor<?, ?, ?>>(
				desc.getUniqueString(), desc));
		assertTrue(evaluator.excludes(group, expr.getExclusions()));

		// test a fast fit - e.g. we don't need to scan the whole group
		group = new Group();
		expr = new GroupExpression("PERSON", "LOCATION", "SCREAMS");
		expr.addExclusion("P*");

		desc = metaData.getDescriptorByValue("PERSON", "Philipp");
		group.append(new GroupEntry<Descriptor<?, ?, ?>>(
				desc.getUniqueString(), desc));
		assertTrue(evaluator.excludes(group, expr.getExclusions()));
		desc = metaData.getDescriptorByValue("LOCATION", "Aachen");
		group.append(new GroupEntry<Descriptor<?, ?, ?>>(
				desc.getUniqueString(), desc));
		assertTrue(evaluator.excludes(group, expr.getExclusions()));

		// check some DimensionalSelectors
		final DimensionSelector sel = new DimensionSelector("PERSON", "GENDER",
				"GENDER");
		expr = new GroupExpression(sel);
		expr.addExclusion("MALE");

		group = new Group();
		group.append(new GroupEntry<DimensionSelector>("MALE", sel));
		assertTrue(evaluator.excludes(group, expr.getExclusions()));
		group = new Group();
		group.append(new GroupEntry<DimensionSelector>("FEMALE", sel));
		assertFalse(evaluator.excludes(group, expr.getExclusions()));

	}

	/**
	 * Tests the exception to be thrown by the
	 * {@link GroupEvaluator#generateGroups(GroupExpression)} implementation,
	 * when an invalid model is used.
	 */
	@Test
	public void testGenerateGroupsWithInvalidModelDescriptor() {
		thrown.expect(ForwardedRuntimeException.class);
		thrown.expect(new ForwardExceptionMatcher(
				GroupEvaluatorException.class, 1000));

		// create the evaluator
		final GroupEvaluator evaluator = new GroupEvaluator(model);

		// check a null descriptorId
		evaluator.generateGroups(new GroupExpression("UNKNOWN"));
	}

	/**
	 * Tests the generation of groups, i.e.
	 * {@link GroupEvaluator#generateGroups(GroupExpression)}.
	 */
	@Test
	public void testGenerateGroups() {
		final MetaDataModel m = model.getMetaDataModel();

		List<Group> res;
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
		for (final Group g : res) {
			assertEquals(2, g.size());
			assertTrue(g.getEntry(0).getMeta() instanceof IntegerDescriptor);
			assertTrue(g.getEntry(1).getMeta() instanceof GeneralDescriptor);
		}

		// generate a group for three descriptors
		group = new GroupExpression("LOCATION", "SCREAMS", "PERSON");
		res = evaluator.generateGroups(group);

		assertEquals(36, res.size());
		for (final Group g : res) {
			assertEquals(3, g.size());

			/*
			 * the 0 position contains also a NullDescriptor as well as a
			 * GeneralDescriptor, therefore we don't test it here
			 */
			assertTrue(g.getEntry(1).getMeta() instanceof IntegerDescriptor);
			assertTrue(g.getEntry(2).getMeta() instanceof GeneralDescriptor);
		}

		// test the exclusion of values
		group = new GroupExpression("SCREAMS", "PERSON");
		group.addExclusion("*", "P*");
		res = evaluator.generateGroups(group);
		assertEquals(9, res.size());
		for (final Group g : res) {

			// the descriptor 'Philipp' should been excluded
			assertTrue(!((Descriptor<?, ?, ?>) g.getEntry(1).getMeta())
					.getUniqueString().equals("Philipp"));
		}

		// test the usage of several exclusions
		group = new GroupExpression("SCREAMS", "PERSON");
		group.addExclusion("*", "P*");
		group.addExclusion("12", "\\**");
		res = evaluator.generateGroups(group);
		assertEquals(8, res.size());
		for (final Group g : res) {

			// the descriptor 'Philipp' should been excluded
			assertTrue(!g.getEntry(1).getLabel().equals("Philipp"));

			// the '*Edison*' with '12' screams should been excluded
			assertTrue(!g.getEntry(1).getLabel().equals("*Edison*")
					|| !g.getEntry(0).getLabel().equals("12"));
		}

		// test a full exclusion
		group = new GroupExpression("SCREAMS", "PERSON");
		group.addExclusion("*");
		res = evaluator.generateGroups(group);
		assertEquals(0, res.size());

		// test the exclusion of null
		group = new GroupExpression("LOCATION", "PERSON");
		group.addExclusion((String) null);
		res = evaluator.generateGroups(group);
		assertEquals(8, res.size());
		for (final Group g : res) {

			// the null location should not be available
			assertNotNull(g.getEntry(0).getLabel());
		}

		group = new GroupExpression("PERSON", "LOCATION");
		group.addExclusion("P*", (String) null);
		res = evaluator.generateGroups(group);
		assertEquals(11, res.size());
		for (final Group g : res) {

			// the null location should not be available
			assertTrue(g.toString(), !g.getEntry(0).getLabel()
					.equals("Philipp")
					|| g.getEntry(1).getLabel() != null);
		}
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
		assertTrue(group.isValid());

		assertNull(evaluator.evaluateGroupExpression(group));
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
		group.setSelectors("PERSON", "LOCATION");
		assertTrue(group.isValid());

		final GroupResult res = evaluator.evaluateGroupExpression(group);
		assertNotNull(res);
		assertEquals(12, res.size());

		// check the 6-entries we should have
		GroupResultEntry entry;
		entry = res.getEntry("Debbie", "Aachen");
		assertTrue(Arrays.binarySearch(entry.getBitmap().getIds(), 4) == 0);
		entry = res.getEntry("*Edison*", "Aachen");
		assertTrue(Arrays.binarySearch(entry.getBitmap().getIds(), 5) == 0);
		entry = res.getEntry("Philipp", null);
		assertTrue(Arrays.binarySearch(entry.getBitmap().getIds(), 2) == 0);
		entry = res.getEntry("Philipp", "Mönchengladbach");
		assertTrue(Arrays.binarySearch(entry.getBitmap().getIds(), 1) == 0);
		entry = res.getEntry("Philipp", "Aachen");
		assertTrue(Arrays.binarySearch(entry.getBitmap().getIds(), 3) == 0);
		entry = res.getEntry("Tobias", "Aachen");
		assertTrue(Arrays.binarySearch(entry.getBitmap().getIds(), 0) == 0);
	}

	/**
	 * Tests the evaluation of a group expression with dimensions.
	 */
	@Test
	public void testEvaluateGroupExpressionWithDimensions() {
		GroupExpression group;
		GroupResult res;
		GroupResultEntry resEntry;

		final GroupEvaluator evaluator = new GroupEvaluator(model);

		// create a simple expression
		group = new GroupExpression();
		group.setSelectors("LOCATION", new DimensionSelector("PERSON",
				"GENDER", "GENDER"));
		group.addExclusion("*", "MALE");
		assertTrue(group.isValid());

		res = evaluator.evaluateGroupExpression(group);
		assertEquals(3, res.size());

		resEntry = res.getEntry("Mönchengladbach", "FEMALE");
		assertEquals(0, resEntry.getBitmap().determineCardinality());

		resEntry = res.getEntry(null, "FEMALE");
		assertEquals(0, resEntry.getBitmap().determineCardinality());

		resEntry = res.getEntry("Aachen", "FEMALE");
		assertEquals(4, resEntry.getBitmap().getIds()[0]);

		// create an expression with no results
		group = new GroupExpression();
		group.setSelectors("LOCATION", new DimensionSelector("PERSON",
				"GENDER", "GENDER"));
		group.addExclusion("*");
		assertTrue(group.isValid());

		res = evaluator.evaluateGroupExpression(group);
		assertEquals(0, res.size());
	}

	private void assertContains(final List<Group> groups,
			final Descriptor<String, ?, ?> desc, final int position) {
		for (final Group group : groups) {
			if (group.getEntry(position).getMeta().equals(desc)) {
				return;
			}
		}

		assertFalse("Unable to find '" + desc + "' in '" + groups
				+ "' at position '" + position + "'.", false);
	}

	/**
	 * CleanUp after each test.
	 */
	@After
	public void cleanup() {

		// cleanup
		model.release(true);
	}
}
