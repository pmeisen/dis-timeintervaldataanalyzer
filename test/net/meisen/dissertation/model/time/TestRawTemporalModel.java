package net.meisen.dissertation.model.time;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;

import java.util.Random;

import net.meisen.dissertation.config.TestConfig;
import net.meisen.dissertation.impl.naturals.IntegerNaturals;
import net.meisen.dissertation.impl.naturals.IntegerNaturalsFactory;
import net.meisen.dissertation.model.granularity.rawtime.Minute;
import net.meisen.dissertation.model.naturals.INaturals;
import net.meisen.dissertation.model.time.RawTemporalModel;
import net.meisen.general.sbconfigurator.api.IConfiguration;
import net.meisen.general.sbconfigurator.runners.JUnitConfigurationRunner;
import net.meisen.general.sbconfigurator.runners.annotations.ContextClass;
import net.meisen.general.sbconfigurator.runners.annotations.ContextFile;
import net.meisen.general.sbconfigurator.runners.annotations.SystemProperty;

import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;

/**
 * Tests the implementation of the class {@code RawTemporalModel}.
 * 
 * @author pmeisen
 * 
 */
@RunWith(JUnitConfigurationRunner.class)
@ContextClass(TestConfig.class)
@ContextFile("test-sbconfigurator-core.xml")
@SystemProperty(property = "testBeans.selector", value = "net/meisen/dissertation/models/impl/time/testRawTemporalModel.xml")
public class TestRawTemporalModel {

	@Autowired(required = true)
	@Qualifier("coreConfiguration")
	private IConfiguration configuration;

	/**
	 * Checks some method of the {@code RawTemporalModel} used with
	 * {@code IntegerNaturals} and a granularity of {@code Minute}. The tested
	 * methods are:
	 * <ul>
	 * <li>{@link RawTemporalModel#getType()}</li>
	 * <li>{@link RawTemporalModel#getStart()}</li>
	 * <li>{@link RawTemporalModel#getEnd()}</li>
	 * <li>{@link RawTemporalModel#getGranularity()}</li>
	 * <li>{@link RawTemporalModel#getNext(INaturals)}</li>
	 * </ul>
	 */
	@Test
	public void testIntegerRawTemporalModel() {
		final IntegerNaturalsFactory factory = new IntegerNaturalsFactory();
		final RawTemporalModel<IntegerNaturals> model = configuration
				.getModule("IntegerNaturalsMinute");

		// check the getType
		assertEquals(IntegerNaturals.class, model.getType());

		// check the getStart
		assertEquals(factory.generate(0), model.getStart());
		assertEquals(factory.getZero(), model.getStart());

		// check the getEnd
		assertEquals(factory.generate(Integer.MAX_VALUE), model.getEnd());
		assertEquals(factory.getMax(), model.getEnd());

		// check the getGranularity to be correct
		assertEquals(new Minute(), model.getGranularity());

		// check the nextGranule of the Model
		assertNull(model.getNext(model.getEnd()));
		assertEquals(factory.generate(11), model.getNext(factory.generate(10)));

		// randomly select an integer and get the next
		final Random rnd = new Random();
		for (int i = 0; i <= Integer.MAX_VALUE; i += rnd.nextInt(100) + 1) {

			// get the overrun
			if (i < 0) {
				i = Integer.MAX_VALUE;
			}

			// get the next value
			final IntegerNaturals current = factory.generate(i);
			final IntegerNaturals next = model.getNext(current);

			// check the result
			if (current.compareTo(model.getEnd()) == 0) {
				assertNull(next);
			} else {
				assertEquals(factory.generate(i + 1), next);
			}

			// make sure it stops
			if (Integer.MAX_VALUE == i) {
				break;
			}
		}
	}

}
