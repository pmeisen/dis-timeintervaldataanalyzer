package net.meisen.dissertation.impl.naturals;

import net.meisen.dissertation.model.naturals.INaturals;


/**
 * Naturals represented by the Java type {@code int}.
 * 
 * @author pmeisen
 * 
 */
public class IntegerNaturals implements INaturals<IntegerNaturals> {
	private int value;

	/**
	 * Default constructor to create a natural number based on the {@code int}.
	 * 
	 * @param value
	 *            the {@code int} value to be assigned to this
	 */
	protected IntegerNaturals(final int value) {
		this.value = value;
	}

	@Override
	public IntegerNaturals subtract(final IntegerNaturals subtrahend) {
		return new IntegerNaturals(value - subtrahend.value);
	}

	@Override
	public IntegerNaturals add(final IntegerNaturals summand) {
		return new IntegerNaturals(value + summand.value);
	}

	@Override
	public boolean equals(final Object comp) {
		if (comp == this) {
			return true;
		} else if (comp instanceof IntegerNaturals) {
			return value == ((IntegerNaturals) comp).value;
		} else if (comp instanceof Integer) {
			return ((Integer) comp).equals(value);
		} else {
			return false;
		}
	}

	@Override
	public String toString() {
		return "" + value;
	}

	@Override
	public int compareTo(final IntegerNaturals o) {
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
