package net.meisen.dissertation.model.descriptors;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;

import java.math.BigDecimal;
import java.math.BigInteger;

import net.meisen.dissertation.model.descriptors.mock.MockDescriptorPrimitiveDataType;

import org.junit.Test;

/**
 * Tests the implementation of {@code DescriptorPrimitiveDataType}.
 * 
 * @author pmeisen
 * 
 */
public class TestDescriptorPrimitiveDataType {

	/**
	 * Tests the mapping to integers.
	 */
	@Test
	public void testIntegerMapping() {
		final MockDescriptorPrimitiveDataType<Integer> m = new MockDescriptorPrimitiveDataType<Integer>(
				new Integer(1));

		assertEquals(new Integer(1), m.mapToDataType(new Integer(1)));
		assertEquals(new Integer(Short.MAX_VALUE),
				m.mapToDataType(Short.MAX_VALUE));

		assertEquals(new Integer(1), m.mapToDataType(new Double(1)));
		assertEquals(new Integer(1000), m.mapToDataType(new Double(1000)));

		assertEquals(new Integer(1), m.mapToDataType(BigInteger.valueOf(1)));

		assertEquals(new Integer(1), m.mapToDataType(BigDecimal.valueOf(1.0)));
		assertEquals(new Integer(1), m.mapToDataType(BigDecimal.valueOf(1)));
		assertEquals(new Integer(1), m.mapToDataType(new BigDecimal(1.0)));
		assertEquals(new Integer(1), m.mapToDataType(new BigDecimal(1)));

		assertNull(m.mapToDataType(Long.MAX_VALUE));
	}

	/**
	 * Tests the mapping to doubles.
	 */
	@Test
	public void testDoubleMapping() {
		final MockDescriptorPrimitiveDataType<Double> m = new MockDescriptorPrimitiveDataType<Double>(
				new Double(1));

		assertEquals(new Double(1), m.mapToDataType(new Double(1)));
		assertEquals(new Double(1), m.mapToDataType(BigInteger.valueOf(1)));
		assertEquals(new Double(1.567), m.mapToDataType(new BigDecimal(1.567)));
		assertEquals(new Double(1.5), m.mapToDataType(BigDecimal.valueOf(1.5)));
		assertEquals(new Double(1), m.mapToDataType(BigDecimal.valueOf(1.0)));
		assertEquals(new Double(1), m.mapToDataType(BigDecimal.valueOf(1)));
		assertEquals(new Double(1), m.mapToDataType(new BigDecimal(1.0)));
		assertEquals(new Double(1), m.mapToDataType(new BigDecimal(1)));
		assertEquals(new Double(Double.MAX_VALUE),
				m.mapToDataType(new BigDecimal(Double.MAX_VALUE)));

		assertNull(m.mapToDataType(new BigDecimal(Long.MAX_VALUE + "."
				+ Long.MAX_VALUE)));
	}
}