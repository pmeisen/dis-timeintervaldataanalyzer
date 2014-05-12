package net.meisen.dissertation.impl.indexes.datarecord.slices;

import static org.junit.Assert.assertEquals;

import java.util.UUID;

import net.meisen.dissertation.model.indexes.datarecord.IntervalIndex;
import net.meisen.dissertation.model.indexes.datarecord.MetaIndexDimension;
import net.meisen.dissertation.model.indexes.datarecord.slices.BitmapId;

import org.junit.Test;

public class TestBitmapId {

	@Test
	public void testSerializationWithoutClassifier() {
		final BitmapId<?> id = new BitmapId<Integer>(5,
				MetaIndexDimension.class);
		assertEquals(Integer.class, id.getIdType());
		assertEquals(5, id.getId());
		assertEquals(MetaIndexDimension.class, id.getType());
		assertEquals("", id.getClassifier());

		final BitmapId<?> deId = new BitmapId<Integer>(id.bytes());
		assertEquals(Integer.class, deId.getIdType());
		assertEquals(5, deId.getId());
		assertEquals(MetaIndexDimension.class, deId.getType());
		assertEquals("", deId.getClassifier());

		assertEquals(id, deId);
		assertEquals(id.hashCode(), deId.hashCode());
	}

	@Test
	public void testSerializationWithClassifier() {
		final BitmapId<?> id = new BitmapId<Byte>((byte) 51,
				IntervalIndex.class, "ModelIdIsALongValueThatIsWhatSoEver");
		assertEquals(Byte.class, id.getIdType());
		assertEquals((byte) 51, id.getId());
		assertEquals(IntervalIndex.class, id.getType());
		assertEquals("ModelIdIsALongValueThatIsWhatSoEver", id.getClassifier());

		final BitmapId<?> deId = new BitmapId<Byte>(id.bytes());
		assertEquals(Byte.class, deId.getIdType());
		assertEquals((byte) 51, deId.getId());
		assertEquals(IntervalIndex.class, deId.getType());
		assertEquals("ModelIdIsALongValueThatIsWhatSoEver",
				deId.getClassifier());

		assertEquals(id, deId);
		assertEquals(id.hashCode(), deId.hashCode());
	}

	@Test
	public void testSerializationWithObjectIdentifier() {
		final String rId = UUID.randomUUID().toString();

		final BitmapId<?> id = new BitmapId<String>(rId, IntervalIndex.class,
				"ModelIdIsALongValueThatIsWhatSoEver");
		assertEquals(String.class, id.getIdType());
		assertEquals(rId, id.getId());
		assertEquals(IntervalIndex.class, id.getType());
		assertEquals("ModelIdIsALongValueThatIsWhatSoEver", id.getClassifier());

		final BitmapId<?> deId = new BitmapId<Byte>(id.bytes());
		assertEquals(String.class, deId.getIdType());
		assertEquals(rId, deId.getId());
		assertEquals(IntervalIndex.class, deId.getType());
		assertEquals("ModelIdIsALongValueThatIsWhatSoEver",
				deId.getClassifier());

		assertEquals(id, deId);
		assertEquals(id.hashCode(), deId.hashCode());
	}
}
