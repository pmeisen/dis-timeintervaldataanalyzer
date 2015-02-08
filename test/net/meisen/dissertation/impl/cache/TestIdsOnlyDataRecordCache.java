package net.meisen.dissertation.impl.cache;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;
import net.meisen.dissertation.help.LoaderBasedTest;
import net.meisen.dissertation.model.data.TidaModel;
import net.meisen.dissertation.model.datasets.IDataRecord;
import net.meisen.dissertation.model.indexes.datarecord.IDataRecordMeta;

import org.junit.Test;

/**
 * Tests the implementation of {@code IdsOnlyDataRecordCache}.
 * 
 * @author pmeisen
 * 
 */
public class TestIdsOnlyDataRecordCache extends LoaderBasedTest {

	/**
	 * Tests the implementation and creation of records using
	 * {@link IdsOnlyDataRecordCache#get(int)}.
	 */
	@Test
	public void testRecordCreation() {
		final TidaModel m = m("/net/meisen/dissertation/impl/cache/idsOnlyDataRecordCache.xml");
		final IDataRecordMeta meta = m.getDataRecordFactory().getMeta();

		/*
		 * Check some records, the method is called indirectly using the index's
		 * getRecord method.
		 */
		IDataRecord rec;

		rec = m.getIndex().getRecord(0);
		
		assertEquals(0, rec.getValue(meta.getPosRecordId()));
		assertEquals(1l, rec.getValue(meta.getPosStart()));
		assertEquals(2l, rec.getValue(meta.getPosEnd()));
		assertEquals(100, rec.getValue(meta.getFirstPosDescModelIds()));

		rec = m.getIndex().getRecord(1);
		assertEquals(1, rec.getValue(meta.getPosRecordId()));
		assertEquals(5l, rec.getValue(meta.getPosStart()));
		assertEquals(5l, rec.getValue(meta.getPosEnd()));
		assertEquals(200, rec.getValue(meta.getFirstPosDescModelIds()));

		rec = m.getIndex().getRecord(2);
		assertEquals(2, rec.getValue(meta.getPosRecordId()));
		assertEquals(1l, rec.getValue(meta.getPosStart()));
		assertEquals(4l, rec.getValue(meta.getPosEnd()));
		assertEquals(300, rec.getValue(meta.getFirstPosDescModelIds()));

		rec = m.getIndex().getRecord(3);
		assertEquals(3, rec.getValue(meta.getPosRecordId()));
		assertEquals(2l, rec.getValue(meta.getPosStart()));
		assertEquals(3l, rec.getValue(meta.getPosEnd()));
		assertEquals(100, rec.getValue(meta.getFirstPosDescModelIds()));
		
		rec = m.getIndex().getRecord(4);
		assertEquals(4, rec.getValue(meta.getPosRecordId()));
		assertEquals(0l, rec.getValue(meta.getPosStart()));
		assertEquals(10l, rec.getValue(meta.getPosEnd()));
		assertEquals(250, rec.getValue(meta.getFirstPosDescModelIds()));
		
		assertNull(m.getIndex().getRecord(5));
	}
}
