package net.meisen.dissertation.impl.parser.query.select.evaluator;

import static org.junit.Assert.assertEquals;

import java.util.Arrays;

import net.meisen.dissertation.config.TestConfig;
import net.meisen.dissertation.config.xslt.DefaultValues;
import net.meisen.dissertation.help.ModuleBasedTest;
import net.meisen.dissertation.impl.parser.query.select.DescriptorComperator;
import net.meisen.dissertation.impl.parser.query.select.logical.DescriptorLeaf;
import net.meisen.dissertation.impl.parser.query.select.logical.RootNode;
import net.meisen.dissertation.model.data.TidaModel;
import net.meisen.dissertation.model.indexes.datarecord.slices.Bitmap;
import net.meisen.general.sbconfigurator.runners.annotations.ContextClass;
import net.meisen.general.sbconfigurator.runners.annotations.ContextFile;

import org.junit.Test;

/**
 * Tests the implementation of the {@code DescriptorLogicEvaluator}.
 * 
 * @author pmeisen
 * 
 */
@ContextClass(TestConfig.class)
@ContextFile("test-sbconfigurator-core.xml")
public class TestDescriptorLogicEvaluator extends ModuleBasedTest {

	/**
	 * Tests the
	 * {@link DescriptorLogicEvaluator#evaluateDescriptorLeaf(DescriptorLeaf)}.
	 */
	@Test
	public void testEvaluateDescriptorLeaf() {
		DescriptorLeaf leaf;
		Bitmap res;

		// initialize the module
		setModulesHolder("/net/meisen/dissertation/impl/parser/query/select/evaluator/testPersonModel.xml");
		final TidaModel model = modulesHolder
				.getModule(DefaultValues.TIDAMODEL_ID);
		model.initialize();
		model.bulkLoadDataFromDataModel();

		// create an evaluator
		final DescriptorLogicEvaluator evaluator = new DescriptorLogicEvaluator(
				model);

		// create one that selects every person starting with T
		leaf = new DescriptorLeaf(new RootNode(), new DescriptorComperator(
				"PERSON", "T*"));
		res = evaluator.evaluateDescriptorLeaf(leaf);
		assertEquals(1, res.determineCardinality());
		assertEquals(0, Arrays.binarySearch(res.getIds(), 0));

		// next create one with all persons starting with P
		leaf = new DescriptorLeaf(new RootNode(), new DescriptorComperator(
				"PERSON", "P*"));
		res = evaluator.evaluateDescriptorLeaf(leaf);
		assertEquals(3, res.determineCardinality());
		assertEquals(0, Arrays.binarySearch(res.getIds(), 1));
		assertEquals(1, Arrays.binarySearch(res.getIds(), 2));
		assertEquals(2, Arrays.binarySearch(res.getIds(), 3));

		// just use the index no wildchar
		leaf = new DescriptorLeaf(new RootNode(), new DescriptorComperator(
				"PERSON", "Philipp"));
		res = evaluator.evaluateDescriptorLeaf(leaf);
		assertEquals(3, res.determineCardinality());
		assertEquals(0, Arrays.binarySearch(res.getIds(), 1));
		assertEquals(1, Arrays.binarySearch(res.getIds(), 2));
		assertEquals(2, Arrays.binarySearch(res.getIds(), 3));

		// use a quoted wildchar
		leaf = new DescriptorLeaf(new RootNode(), new DescriptorComperator(
				"PERSON", "\\*Edison\\*"));
		res = evaluator.evaluateDescriptorLeaf(leaf);
		assertEquals(1, res.determineCardinality());
		assertEquals(0, Arrays.binarySearch(res.getIds(), 5));

		// use wildchars and quoted once
		leaf = new DescriptorLeaf(new RootNode(), new DescriptorComperator(
				"PERSON", "\\*Edi*\\*"));
		res = evaluator.evaluateDescriptorLeaf(leaf);
		assertEquals(1, res.determineCardinality());
		assertEquals(0, Arrays.binarySearch(res.getIds(), 5));

		// cleanup
		model.release(true);
	}
}
