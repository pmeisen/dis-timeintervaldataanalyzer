package net.meisen.dissertation.models.impl.naturals;

import java.math.BigInteger;

import net.meisen.dissertation.models.INaturals;

/**
 * Naturals represented by the Java type {@code BigInteger}.
 * 
 * @author pmeisen
 * 
 */
public class BigIntegerNaturals implements INaturals<BigIntegerNaturals> {
	private BigInteger value;

	/**
	 * Default constructor to create a natural number based on the
	 * {@code BigInteger}.
	 * 
	 * @param value
	 *            the {@code BigInteger} value to be assigned to this
	 */
	protected BigIntegerNaturals(final BigInteger value) {
		this.value = value;
	}

	/**
	 * Default constructor to create a natural number based on the {@code long}.
	 * 
	 * @param value
	 *            the {@code long} value to be assigned to this
	 */
	protected BigIntegerNaturals(final long value) {
		this(BigInteger.valueOf(value));
	}

	/**
	 * Default constructor to create a natural number based on the {@code int}.
	 * 
	 * @param value
	 *            the {@code int} value to be assigned to this
	 */
	protected BigIntegerNaturals(final int value) {
		this(BigInteger.valueOf(value));
	}

	/**
	 * Default constructor to create a natural number based on the
	 * {@code String}.
	 * 
	 * @param value
	 *            the {@code String} value to be assigned to this
	 */
	protected BigIntegerNaturals(final String value) {
		this(new BigInteger(value));
	}

	@Override
	public BigIntegerNaturals subtract(final BigIntegerNaturals subtrahend) {
		return new BigIntegerNaturals(value.subtract(subtrahend.value));
	}

	@Override
	public BigIntegerNaturals add(final BigIntegerNaturals summand) {
		return new BigIntegerNaturals(value.add(summand.value));
	}

	@Override
	public boolean equals(final Object comp) {
		if (comp == this) {
			return true;
		} else if (comp instanceof BigIntegerNaturals) {
			return value.equals(((BigIntegerNaturals) comp).value);
		} else if (comp instanceof Long) {
			return value.equals(BigInteger.valueOf((Long) comp));
		} else if (comp instanceof Integer) {
			return value.equals(BigInteger.valueOf((Integer) comp));
		} else {
			return false;
		}
	}

	@Override
	public String toString() {
		return value.toString();
	}

	@Override
	public int compareTo(final BigIntegerNaturals o) {
		return this.value.compareTo(o.value);
	}
}
