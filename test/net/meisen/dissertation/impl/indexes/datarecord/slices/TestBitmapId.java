package net.meisen.dissertation.impl.indexes.datarecord.slices;

import static org.junit.Assert.assertEquals;

import java.util.UUID;

import net.meisen.dissertation.help.ExceptionBasedTest;
import net.meisen.dissertation.model.indexes.datarecord.IntervalIndex;
import net.meisen.dissertation.model.indexes.datarecord.MetaIndexDimension;
import net.meisen.dissertation.model.indexes.datarecord.slices.BitmapId;

import org.junit.Test;

import com.google.common.base.Strings;

/**
 * Tests the implementation of a {@code BitmapId}.
 * 
 * @author pmeisen
 * 
 */
public class TestBitmapId extends ExceptionBasedTest {

	/**
	 * Tests the serialization without any classifier.
	 */
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

	/**
	 * Tests the serialization with a classifier.
	 */
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

	/**
	 * Tests the serialization with an object as identifier.
	 */
	@Test
	public void testSerializationWithObjectIdentifier() {
		final UUID rId = UUID.randomUUID();

		final BitmapId<?> id = new BitmapId<UUID>(rId, IntervalIndex.class,
				"ModelIdIsALongValueThatIsWhatSoEver");
		assertEquals(UUID.class, id.getIdType());
		assertEquals(rId, id.getId());
		assertEquals(IntervalIndex.class, id.getType());
		assertEquals("ModelIdIsALongValueThatIsWhatSoEver", id.getClassifier());

		final BitmapId<?> deId = new BitmapId<Byte>(id.bytes());
		assertEquals(UUID.class, deId.getIdType());
		assertEquals(rId, deId.getId());
		assertEquals(IntervalIndex.class, deId.getType());
		assertEquals("ModelIdIsALongValueThatIsWhatSoEver",
				deId.getClassifier());

		assertEquals(id, deId);
		assertEquals(id.hashCode(), deId.hashCode());
	}

	/**
	 * Tests the maximum size of a byte-representation.
	 */
	@Test
	public void testMaximumSize() {
		final String fullId = Strings.repeat("A", Byte.MAX_VALUE - 1);
		final String fullClassifier = Strings.repeat("A", Byte.MAX_VALUE);

		final BitmapId<?> id = new BitmapId<String>(fullId,
				IntervalIndex.class, fullClassifier);
		assertEquals(fullId, id.getId());

		final BitmapId<?> deId = new BitmapId<Byte>(id.bytes());
		assertEquals(String.class, deId.getIdType());
		assertEquals(fullId, deId.getId());
		assertEquals(fullClassifier, id.getClassifier());
	}

	/**
	 * Tests the exception to be thrown if the identifier is to long.
	 */
	@Test
	public void testExceptionWhenSizeOfIdIsExceeded() {
		final String full = Strings.repeat("A", Byte.MAX_VALUE);

		thrown.expect(IllegalArgumentException.class);
		thrown.expectMessage("identifier '" + full + "' is too long");

		final BitmapId<?> id = new BitmapId<String>(full, IntervalIndex.class,
				full);
		id.bytes();
	}

	/**
	 * Tests the exception to be thrown if the classifier is to long.
	 */
	@Test
	public void testExceptionWhenSizeOfClassifierIsExceeded() {
		final String full = Strings.repeat("A", Byte.MAX_VALUE + 1);

		thrown.expect(IllegalArgumentException.class);
		thrown.expectMessage("classifier '" + full + "' is too long");

		final BitmapId<?> id = new BitmapId<String>("A", IntervalIndex.class,
				full);
		id.bytes();
	}
}
