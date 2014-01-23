package net.meisen.dissertation.impl.naturals;

import net.meisen.dissertation.model.naturals.INaturals;


/**
 * Naturals represented by the Java type {@code long}.
 * 
 * @author pmeisen
 * 
 */
public class LongNaturals implements INaturals<LongNaturals> {
	private long value;

	/**
	 * Default constructor to create a natural number based on the {@code long}.
	 * 
	 * @param value
	 *            the {@code long} value to be assigned to this
	 */
	protected LongNaturals(final long value) {
		this.value = value;
	}

	/**
	 * Default constructor to create a natural number based on the {@code int}.
	 * 
	 * @param value
	 *            the {@code int} value to be assigned to this
	 */
	protected LongNaturals(final int value) {
		this.value = (long) value;
	}

	@Override
	public LongNaturals subtract(final LongNaturals subtrahend) {
		return new LongNaturals(value - subtrahend.value);
	}

	@Override
	public LongNaturals add(final LongNaturals summand) {
		return new LongNaturals(value + summand.value);
	}

	@Override
	public boolean equals(final Object comp) {
		if (comp == this) {
			return true;
		} else if (comp instanceof LongNaturals) {
			return value == ((LongNaturals) comp).value;
		} else if (comp instanceof Long) {
			return ((Long) comp).equals(value);
		} else if (comp instanceof Integer) {
			final Integer compInt = (Integer) comp;
			return value == compInt.longValue();
		} else {
			return false;
		}
	}

	@Override
	public String toString() {
		return "" + value;
	}

	@Override
	public int compareTo(final LongNaturals o) {
		if (o == null) {
			throw new NullPointerException(
					"According to the compareTo interface 'e.compareTo(null) should throw a NullPointerException'");
		} else if (this.value < o.value) {
			return -1;
		} else if (this.value > o.value) {
			return 1;
		} else {
			return 0;
		}
	}
}
