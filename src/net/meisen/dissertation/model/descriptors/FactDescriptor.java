package net.meisen.dissertation.model.descriptors;

import net.meisen.general.genmisc.types.Objects;

/**
 * A {@code FactDescriptor} is a {@code Descriptor} which contains the fact
 * information of a descriptor. The information is only available if the
 * descriptor is {@code invariant}, i.e. {@code record-} or
 * {@code value-invariant}. Otherwise the provided value of {@link #getFact()}
 * is considered to be invalid.
 * 
 * @author pmeisen
 * 
 * @param <I>
 *            the type of the identifier of the descriptor's identifier
 */
public class FactDescriptor<I> implements Comparable<FactDescriptor<I>> {
	private Integer hashCode;

	private final String descModelId;
	private final I descId;
	private final boolean recordInvariant;
	private final boolean valueInvariant;
	private final double fact;

	/**
	 * Generates a {@code recordVariant} fact-descriptor.
	 * 
	 * @param descModelId
	 *            the identifier of the model of the {@code Descriptor}
	 * @param descId
	 *            the identifier of the {@code Descriptor}
	 */
	public FactDescriptor(final String descModelId, final I descId) {
		this.descModelId = descModelId;
		this.descId = descId;
		this.fact = -1;
		this.recordInvariant = false;
		this.valueInvariant = false;
	}

	/**
	 * Generates a {@code recordInvariant} fact-descriptor.
	 * 
	 * @param descModelId
	 *            the identifier of the model of the {@code Descriptor}
	 * @param descId
	 *            the identifier of the {@code Descriptor}
	 * @param fact
	 *            the invariant fact
	 */
	public FactDescriptor(final String descModelId, final I descId,
			final double fact) {
		this.descModelId = descModelId;
		this.descId = descId;
		this.fact = fact;
		this.recordInvariant = true;
		this.valueInvariant = descId == null;
	}

	/**
	 * Constructor to create a value invariant record.
	 * 
	 * @param descModelId
	 *            the identifier of the {@code DescriptorModel}
	 * @param fact
	 *            the fact value
	 */
	public FactDescriptor(final String descModelId, final double fact) {
		this.descModelId = descModelId;
		this.descId = null;
		this.fact = fact;
		this.recordInvariant = true;
		this.valueInvariant = true;
	}

	/**
	 * Gets the identifier of the model.
	 * 
	 * @return the identifier of the model
	 */
	public String getModelId() {
		return descModelId;
	}

	/**
	 * Gets the identifier of the {@code Descriptor}.
	 * 
	 * @return the identifier of the {@code Descriptor}
	 */
	public I getId() {
		return descId;
	}

	/**
	 * {@code true} if the fact is record invariant, otherwise {@code false}.
	 * 
	 * @return {@code true} if the fact is record invariant, otherwise
	 *         {@code false}
	 */
	public boolean isRecordInvariant() {
		return recordInvariant;
	}

	/**
	 * {@code true} if the fact is value invariant, otherwise {@code false}.
	 * 
	 * @return {@code true} if the fact is value invariant, otherwise
	 *         {@code false}
	 */
	public boolean isValueInvariant() {
		return valueInvariant;
	}

	/**
	 * Checks if the {@code FactDescriptor} is variant considering it's value
	 * (and the record, i.e. the value might change for each record).
	 * 
	 * @return {@code true} if the value is record variant, otherwise
	 *         {@code false}
	 */
	public boolean isVariant() {
		return !recordInvariant && !valueInvariant;
	}

	/**
	 * Gets the fact, might be an invalid value, i.e. if the {@code Descriptor}
	 * is {@code recordVariant}.
	 * 
	 * @return the fact, might be an invalid value, i.e. if the
	 *         {@code Descriptor} is {@code recordVariant}
	 */
	public double getFact() {
		return fact;
	}

	@Override
	public int hashCode() {
		if (hashCode == null) {
			hashCode = Objects.generateHashCode(13, 97, getId(), getModelId());
		}

		return hashCode.intValue();
	}

	@Override
	public boolean equals(final Object obj) {
		if (obj == this) {
			return true;
		} else if (obj instanceof FactDescriptor) {
			final FactDescriptor<?> fd = (FactDescriptor<?>) obj;

			return hashCode() == fd.hashCode()
					&& Objects.equals(fd.getId(), getId())
					&& Objects.equals(fd.getModelId(), getModelId());
		} else {
			return false;
		}
	}

	@Override
	public String toString() {
		return getModelId() + "." + getId() + " = " + getFact();
	}

	@Override
	public int compareTo(final FactDescriptor<I> factDesc) {

		// A negative integer, zero, or a positive integer as this object is
		// less than, equal to, or greater than the specified object.

		if (factDesc == null) {
			throw new NullPointerException(
					"Null descriptors are not supported!");
		}

		// check if the objects are equal
		final boolean equal = equals(factDesc);
		if (equal) {
			return 0;
		}

		// both are variant
		if (isVariant() && factDesc.isVariant()) {
			/*
			 * the models are equal within one set, which is ensured by the
			 * implementation, therefore just check the identifiers
			 */
			return Objects.compare(getId(), factDesc.getId());
		}
		// one is variant the other isn't
		else if (isVariant()) {
			return -1;
		} else if (factDesc.isVariant()) {
			return 1;
		}
		// both are invariant, the fact value counts
		else {
			final double fact1 = getFact();
			final double fact2 = factDesc.getFact();

			if (fact1 < fact2) {
				return -1;
			} else if (fact1 > fact2) {
				return 1;
			}

			/*
			 * the models are equal within one set, which is ensured by the
			 * implementation, therefore just check the identifiers
			 */
			return Objects.compare(getId(), factDesc.getId());
		}
	}
}
