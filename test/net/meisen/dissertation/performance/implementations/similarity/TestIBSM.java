package net.meisen.dissertation.performance.implementations.similarity;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

import java.util.Date;
import java.util.Iterator;

import net.meisen.dissertation.config.TidaConfig;
import net.meisen.dissertation.impl.parser.query.DateIntervalValue;
import net.meisen.dissertation.impl.parser.query.Interval;
import net.meisen.dissertation.impl.parser.query.IntervalType;
import net.meisen.dissertation.model.data.IntervalModel;
import net.meisen.dissertation.model.data.TidaModel;
import net.meisen.dissertation.model.handler.TidaModelHandler;
import net.meisen.dissertation.model.time.mapper.BaseMapper;
import net.meisen.dissertation.performance.implementations.model.DataHolder;
import net.meisen.general.genmisc.types.Dates;
import net.meisen.general.sbconfigurator.runners.JUnitConfigurationRunner;
import net.meisen.general.sbconfigurator.runners.annotations.ContextClass;
import net.meisen.general.sbconfigurator.runners.annotations.ContextFile;

import org.junit.Before;
import org.junit.Ignore;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.beans.factory.annotation.Autowired;

/**
 * Tests the implementation of the IBSM, adapted considering the dynamic
 * e-sequences creation based on a {@code SelectQuery}.
 * 
 * @author pmeisen
 * 
 */
@ContextClass(TidaConfig.class)
@ContextFile("sbconfigurator-core.xml")
@RunWith(JUnitConfigurationRunner.class)
public class TestIBSM {
	private final static String modelPath = "/net/meisen/dissertation/performance/implementations/model/tida-model-minute.xml";

	@Autowired
	private TidaModelHandler loader;

	private TidaModel model;

	@Before
	public void initialize() {
		loader.unloadAll();

		model = loader
				.loadViaXslt("/net/meisen/dissertation/performance/implementations/model/tida-model-minute.xml");
	}

	/**
	 * Tests the creation of an {@code ESequenceDefinition} based on an
	 * interval, i.e.
	 * {@link ESequenceDefinition#ESequenceDefinition(IntervalModel, Interval)}.
	 */
	@Test
	public void testESequenceDefinitionCreation() {
		final IntervalType in = IntervalType.INCLUDE;
		final IntervalType ex = IntervalType.EXCLUDE;
		final String tz = Dates.GENERAL_TIMEZONE;

		// the instances we need
		Interval<?> interval;
		DateIntervalValue val1, val2;
		ESequenceDefinition esd;

		// create some different instances and validate
		// start="01.01.2008 00:00:00", end="30.12.2008 23:59:00", and
		// granularity="MINUTE"
		val1 = new DateIntervalValue(Dates.isDate("20.01.2008 08:00:00", tz));
		val2 = new DateIntervalValue(Dates.isDate("20.01.2008 08:07:00", tz));
		interval = new Interval<Date>(val1, ex, val2, in);
		esd = new ESequenceDefinition(model.getIntervalModel(), interval);

		assertEquals(0l, esd.getStart());
		assertEquals(525599l, esd.getEnd());
		assertEquals(27841l, esd.getWindowStart());
		assertEquals(27847l, esd.getWindowEnd());
		assertEquals(7l, esd.getWindowSize());

		val1 = new DateIntervalValue(Dates.isDate("20.01.2008 08:00:00", tz));
		val2 = new DateIntervalValue(Dates.isDate("20.01.2008 08:07:00", tz));
		interval = new Interval<Date>(val1, ex, val2, ex);
		esd = new ESequenceDefinition(model.getIntervalModel(), interval);

		assertEquals(0l, esd.getStart());
		assertEquals(525599l, esd.getEnd());
		assertEquals(27841l, esd.getWindowStart());
		assertEquals(27846l, esd.getWindowEnd());
		assertEquals(6l, esd.getWindowSize());

		val1 = new DateIntervalValue(Dates.isDate("20.01.2008 08:00:00", tz));
		val2 = new DateIntervalValue(Dates.isDate("20.01.2008 08:07:00", tz));
		interval = new Interval<Date>(val1, in, val2, in);
		esd = new ESequenceDefinition(model.getIntervalModel(), interval);

		assertEquals(0l, esd.getStart());
		assertEquals(525599l, esd.getEnd());
		assertEquals(27840l, esd.getWindowStart());
		assertEquals(27847l, esd.getWindowEnd());
		assertEquals(8l, esd.getWindowSize());

		val1 = new DateIntervalValue(Dates.isDate("20.01.2008 08:00:00", tz));
		val2 = new DateIntervalValue(Dates.isDate("20.01.2008 08:07:00", tz));
		interval = new Interval<Date>(val1, in, val2, ex);
		esd = new ESequenceDefinition(model.getIntervalModel(), interval);

		assertEquals(0l, esd.getStart());
		assertEquals(525599l, esd.getEnd());
		assertEquals(27840l, esd.getWindowStart());
		assertEquals(27846l, esd.getWindowEnd());
		assertEquals(7l, esd.getWindowSize());
	}

	/**
	 * Tests the iteration over an {@code ESequenceDefinition}, i.e.
	 * {@link ESequenceDefinition#iterator()}.
	 */
	@Test
	public void testESequenceDefinitionIteration() {
		final IntervalType in = IntervalType.INCLUDE;
		final IntervalType ex = IntervalType.EXCLUDE;
		final String tz = Dates.GENERAL_TIMEZONE;

		final IntervalModel im = model.getIntervalModel();

		// the instances we need
		Interval<?> interval;
		DateIntervalValue val1, val2;

		// create some different instances and validate
		// start="01.01.2008 00:00:00", end="30.12.2008 23:59:00", and
		// granularity="MINUTE"
		val1 = new DateIntervalValue(Dates.isDate("01.01.2008", tz));
		val2 = new DateIntervalValue(Dates.isDate("02.01.2008", tz));
		interval = new Interval<Date>(val1, in, val2, ex);
		assertIterator(new ESequenceDefinition(im, interval), 1440, 1440);

		val1 = new DateIntervalValue(Dates.isDate("01.02.2008 00:00:00", tz));
		val2 = new DateIntervalValue(Dates.isDate("02.02.2008", tz));
		interval = new Interval<Date>(val1, in, val2, ex);
		assertIterator(new ESequenceDefinition(im, interval), 1440, 1440);

		val1 = new DateIntervalValue(Dates.isDate("01.01.2008 00:01:00", tz));
		val2 = new DateIntervalValue(Dates.isDate("02.01.2008 00:01:00", tz));
		interval = new Interval<Date>(val1, in, val2, ex);
		assertIterator(new ESequenceDefinition(im, interval), 1, 1439);

		val1 = new DateIntervalValue(Dates.isDate("01.01.2008 00:01:00", tz));
		val2 = new DateIntervalValue(Dates.isDate("02.01.2008", tz));
		interval = new Interval<Date>(val1, in, val2, ex);
		assertIterator(new ESequenceDefinition(im, interval), 1, 364);

		val1 = new DateIntervalValue(Dates.isDate("20.01.2008 08:00:00", tz));
		val2 = new DateIntervalValue(Dates.isDate("20.01.2008 08:07:00", tz));
		interval = new Interval<Date>(val1, ex, val2, in);
		assertIterator(new ESequenceDefinition(im, interval), 2, 3);
	}

	private void assertIterator(final ESequenceDefinition esd,
			final long startSize, final long endSize) {
		final BaseMapper<?> mapper = model.getIntervalModel()
				.getTimelineMapper();

		final Iterator<long[]> it = esd.iterator();
		boolean foundStart = false, foundEnd = false;
		while (it.hasNext()) {
			long[] next = it.next();
			final String output = "["
					+ next[0]
					+ ","
					+ next[1]
					+ "] "
					+ "["
					+ Dates.formatDate((Date) mapper.resolve(next[0]),
							"dd.MM.yyyy HH:mm:ss")
					+ ","
					+ Dates.formatDate((Date) mapper.resolve(next[1]),
							"dd.MM.yyyy HH:mm:ss") + "]";

			boolean isStart = next[0] == esd.getStart();
			boolean isEnd = next[1] == esd.getEnd();

			if (isStart || isEnd) {
				if (isStart) {
					assertEquals(output, startSize, next[1] - next[0] + 1);
				}
				if (isEnd) {
					assertEquals(output, endSize, next[1] - next[0] + 1);
				}
				foundStart = foundStart || isStart;
				foundEnd = foundEnd || isEnd;
			} else {
				assertEquals(output, esd.getWindowSize(), next[1] - next[0] + 1);
				assertTrue(esd.isValidSize(next));
			}
		}

		assertTrue(foundStart);
		assertTrue(foundEnd);
	}

	@Test
	public void testEventTableCreation() {
		final EventTable eventTable = new EventTable(100);

		final Object[] label1 = new Object[] { "A", "B", 5 };
		final Object[] label2 = new Object[] { "A", "B", 6 };

		eventTable.addEvent(0, 50, label1);
		eventTable.addEvent(25, 50, label1);
		eventTable.addEvent(25, 50, label2);
		eventTable.addEvent(90, 105, label2);

		// check some border values
		assertEquals(0, eventTable.get(51, label1));
		assertEquals(1, eventTable.get(0, label1));
		assertEquals(1, eventTable.get(24, label1));
		assertEquals(2, eventTable.get(25, label1));
		assertEquals(2, eventTable.get(50, label1));
		assertEquals(0, eventTable.get(90, label1));
		assertEquals(0, eventTable.get(99, label1));
		assertEquals(1, eventTable.get(99, label2));
		assertEquals(1, eventTable.get(25, label2));
		assertEquals(1, eventTable.get(90, label2));
		assertEquals(0, eventTable.get(89, label2));
	}

	@Ignore
	@Test
	public void testIbsm() {
		final DataHolder holder = new DataHolder(model);

		final IBSM ibsm = new IBSM(model, holder.getRecords(100),
				"INTERVAL_START", "INTERVAL_END", "PERSON", "TASKTYPE");

	}
}
