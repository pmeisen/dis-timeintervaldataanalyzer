package net.meisen.dissertation.impl.indexes.datarecord.slices;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;
import net.meisen.dissertation.model.indexes.BaseIndexFactory;
import net.meisen.dissertation.model.indexes.datarecord.slices.Bitmap;

import org.junit.Test;
import org.mockito.Mockito;
import org.mockito.invocation.InvocationOnMock;
import org.mockito.stubbing.Answer;

/**
 * Tests the implementation of {@link RoaringBitmap}.
 * 
 * @author pmeisen
 * 
 */
public class TestRoaringBitmap {

	final BaseIndexFactory factory = Mockito.mock(BaseIndexFactory.class);

	/**
	 * Constructs a factory which creates {@link RoaringBitmap} instances.
	 */
	public TestRoaringBitmap() {
		Mockito.when(factory.createBitmap()).thenAnswer(
				new Answer<RoaringBitmap>() {
					public RoaringBitmap answer(
							final InvocationOnMock invocation) throws Throwable {
						return new RoaringBitmap();
					}
				});
	}

	/**
	 * Tests an empty bitmap.
	 */
	@Test
	public void testEmptyBitmap() {
		final Bitmap bmp = Bitmap.createBitmap(factory);
		assertTrue(bmp instanceof RoaringBitmap);
		assertEquals(0, bmp.determineCardinality());
		assertFalse(bmp.copy() == bmp);
		assertEquals(bmp, bmp.copy());
	}

	/**
	 * Tests the set.
	 */
	@Test
	public void testSet() {
		final RoaringBitmap bitmap = new RoaringBitmap();

		bitmap.set(1);
		bitmap.set(0);
		assertEquals(2, bitmap.determineCardinality());

		bitmap.set(0, 2, 4, 6);
		assertEquals(5, bitmap.determineCardinality());
	}

	/**
	 * Tests the and.
	 */
	@Test
	public void testAnd() {
		final Bitmap bmp = Bitmap.createBitmap(factory, 1, 2, 3, 4, 5, 10, 11,
				12, 13, 14, 15);

		assertEquals(Bitmap.createBitmap(factory),
				bmp.and(Bitmap.createBitmap(factory)));

		assertEquals(Bitmap.createBitmap(factory, 1),
				bmp.and(Bitmap.createBitmap(factory, 1)));

		assertEquals(Bitmap.createBitmap(factory),
				bmp.and(Bitmap.createBitmap(factory, 100)));

		assertEquals(Bitmap.createBitmap(factory, 2, 3, 4),
				bmp.and(Bitmap.createBitmap(factory, 4, 100, 2, 3)));
	}

	/**
	 * Tests the or.
	 */
	@Test
	public void testOr() {
		final Bitmap bmp = Bitmap.createBitmap(factory, 1, 2, 3, 4, 5, 10, 11,
				12, 13, 14, 15);

		assertFalse(bmp == bmp.or(Bitmap.createBitmap(factory)));
		assertEquals(bmp, bmp.or(Bitmap.createBitmap(factory)));

		assertEquals(bmp, bmp.or(Bitmap.createBitmap(factory, 1)));

		assertEquals(Bitmap.createBitmap(factory, 1, 2, 3, 4, 5, 10, 11, 12,
				13, 14, 15, 100), bmp.or(Bitmap.createBitmap(factory, 100)));

		assertEquals(Bitmap.createBitmap(factory, 1, 2, 3, 4, 5, 10, 11, 12,
				13, 14, 15, 100), bmp.or(Bitmap.createBitmap(factory, 4, 100,
				2, 3)));
	}

	/**
	 * Tests the xor.
	 */
	@Test
	public void testXor() {
		final Bitmap bmp = Bitmap.createBitmap(factory, 1, 2, 3, 4, 5, 10, 11,
				12, 13, 14, 15);

		assertFalse(bmp == bmp.xor(Bitmap.createBitmap(factory)));
		assertEquals(bmp, bmp.xor(Bitmap.createBitmap(factory)));

		assertEquals(Bitmap.createBitmap(factory, 2, 3, 4, 5, 10, 11, 12, 13,
				14, 15), bmp.xor(Bitmap.createBitmap(factory, 1)));

		assertEquals(Bitmap.createBitmap(factory, 1, 2, 3, 4, 5, 10, 11, 12,
				13, 14, 15, 100), bmp.or(Bitmap.createBitmap(factory, 100)));

		assertEquals(
				Bitmap.createBitmap(factory, 1, 5, 10, 11, 12, 13, 14, 15, 100),
				bmp.xor(Bitmap.createBitmap(factory, 4, 100, 2, 3)));
	}

	/**
	 * Tests the invertion.
	 */
	@Test
	public void testInvert() {
		final Bitmap bmp = Bitmap.createBitmap(factory, 1, 2, 3, 4, 5, 10, 11,
				12, 13, 14, 15);

		assertEquals(Bitmap.createBitmap(factory, 0, 6, 7, 8, 9),
				bmp.invert(15));
		assertEquals(Bitmap.createBitmap(factory, 0, 10, 11, 12, 13, 14, 15),
				bmp.invert(5));
	}
}
