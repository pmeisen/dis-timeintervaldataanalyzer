package net.meisen.dissertation.impl.measures;

import static org.junit.Assert.assertEquals;
import net.meisen.dissertation.help.LoaderBasedTest;
import net.meisen.dissertation.model.data.TidaModel;
import net.meisen.dissertation.model.indexes.datarecord.bitmap.Bitmap;
import net.meisen.dissertation.model.indexes.datarecord.slices.SliceWithDescriptors;

import org.junit.Test;

public class TestMapFactsHolder extends LoaderBasedTest {

	private TidaModel model = null;

	private MapFactsHolder createMap(final int sliceId) {

		if (model == null) {
			final String xml = "/net/meisen/dissertation/impl/measures/testNumberModel.xml";
			model = m(xml);
		}

		// check the amount of data
		assertEquals(5, model.getAmountOfRecords());

		// create a bitmap which selects all data
		Bitmap bmp = model.getIndexFactory().createBitmap();
		bmp = bmp.invert(model.getAmountOfRecords());

		// get the slice
		final SliceWithDescriptors<?>[] slices = model.getIndex()
				.getIntervalIndexSlices(sliceId, sliceId, true, true);
		assertEquals(1, slices.length);

		// create the map
		final MapFactsHolder map = new MapFactsHolder();
		map.init(slices[0].getDescriptors("NUMBER"), model.getIndex(),
				slices[0].getBitmap());

		return map;
	}

	@Test
	public void testSimpleUsageOfArray() {
		final MapFactsHolder map = new MapFactsHolder();
		map.init(5, -1);

		map.set(5, 3.0);
		assertEquals(1, map.amountOfFacts());
		assertEquals(3.0, map.getFactOfRecord(5), 0.0);
		map.set(2, 1.1);
		assertEquals(2, map.amountOfFacts());
		assertEquals(1.1, map.getFactOfRecord(2), 0.0);
		map.set(1, 3.2);
		assertEquals(3, map.amountOfFacts());
		assertEquals(3.2, map.getFactOfRecord(1), 0.0);

		// check the size
		assertEquals(3, map.recordIds().length);
		assertEquals(3, map.facts().length);

		// check the facts
		final double[] facts = map.sortedFacts();
		assertEquals(1.1, facts[0], 0.0);
		assertEquals(3.0, facts[1], 0.0);
		assertEquals(3.2, facts[2], 0.0);
	}

	@Test
	public void testUsageOfDescriptors() {
		MapFactsHolder map;

		map = createMap(1);
		assertEquals(110.0, map.getFactOfRecord(0), 0.0);
		assertEquals(100.0, map.getFactOfRecord(1), 0.0);
		assertEquals(Double.NaN, map.getFactOfRecord(2), 0.0);
		assertEquals(Double.NaN, map.getFactOfRecord(3), 0.0);
		assertEquals(Double.NaN, map.getFactOfRecord(4), 0.0);
		assertEquals(2, map.amountOfFacts());
		assertEquals(4, map.maxRecordId());

		map = createMap(2);
		assertEquals(110.0, map.getFactOfRecord(0), 0.0);
		assertEquals(100.0, map.getFactOfRecord(1), 0.0);
		assertEquals(130.0, map.getFactOfRecord(2), 0.0);
		assertEquals(Double.NaN, map.getFactOfRecord(3), 0.0);
		assertEquals(Double.NaN, map.getFactOfRecord(4), 0.0);
		assertEquals(3, map.amountOfFacts());
		assertEquals(4, map.maxRecordId());

		map = createMap(5);
		assertEquals(Double.NaN, map.getFactOfRecord(0), 0.0);
		assertEquals(Double.NaN, map.getFactOfRecord(1), 0.0);
		assertEquals(Double.NaN, map.getFactOfRecord(2), 0.0);
		assertEquals(110.0, map.getFactOfRecord(3), 0.0);
		assertEquals(110.0, map.getFactOfRecord(4), 0.0);
		assertEquals(2, map.amountOfFacts());
		assertEquals(4, map.maxRecordId());

		assertEquals(2, map.sortedFacts().length);
	}

	@Test
	public void testInvalidInitOfArray() {
		thrown.expect(IllegalArgumentException.class);
		thrown.expectMessage("maxRecordId must be larger or equal to 0");

		final MapFactsHolder map = new MapFactsHolder();
		map.init(-1, -1);
	}

	@Test
	public void testInvalidRecordIdOfArray() {
		thrown.expect(IllegalArgumentException.class);
		thrown.expectMessage("maximal record identifier can be '10'");

		final MapFactsHolder map = new MapFactsHolder();
		map.init(10, -1);
		map.set(11, 100.0);
	}
}
