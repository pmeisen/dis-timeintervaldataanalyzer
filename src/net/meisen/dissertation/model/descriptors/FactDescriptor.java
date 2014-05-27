package net.meisen.dissertation.model.descriptors;

import net.meisen.general.genmisc.types.Objects;

public class FactDescriptor<I> implements Comparable<FactDescriptor<I>> {
	private final String descModelId;
	private final I descId;
	private final boolean recordInvariant;
	private final double fact;

	public FactDescriptor(final String descModelId, final I descId) {
		this.descModelId = descModelId;
		this.descId = descId;
		this.fact = -1;
		this.recordInvariant = false;
	}

	public FactDescriptor(final String descModelId, final I descId,
			final double fact) {
		this.descModelId = descModelId;
		this.descId = descId;
		this.fact = fact;
		this.recordInvariant = true;
	}

	public String getModelId() {
		return descModelId;
	}

	public I getId() {
		return descId;
	}

	public boolean isRecordInvariant() {
		return recordInvariant;
	}

	public double getFact() {
		return fact;
	}

	@Override
	public int hashCode() {
		if (isRecordInvariant()) {
			return Objects.generateHashCode(13, 97, getId(), getModelId(),
					getFact());
		} else {
			return Objects.generateHashCode(13, 97, getId(), getModelId());
		}
	}

	@Override
	public boolean equals(final Object obj) {
		if (obj == this) {
			return true;
		} else if (obj instanceof FactDescriptor) {
			final FactDescriptor<?> fd = (FactDescriptor<?>) obj;

			return Objects.equals(fd.getId(), getId())
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

		if (factDesc == null) {
			throw new NullPointerException(
					"Null descriptors are not supported!");
		}

		// check if the objects are equal
		final boolean equal = equals(factDesc);
		if (equal) {
			return 0;
		}

		// make sure both are invariant
		final boolean invariantDesc1 = isRecordInvariant();
		final boolean invariantDesc2 = factDesc.isRecordInvariant();
		if (invariantDesc1 && invariantDesc2) {
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
		// both are variant
		else if (!invariantDesc1 && !invariantDesc2) {
			return Objects.compare(getId(), factDesc.getId());
		}
		// invariantDesc1 == true && invariantDesc2 == false
		else if (invariantDesc1) {
			return 1;
		}
		// invariantDesc2 == true && invariantDesc1 == false
		else {
			return -1;
		}
	}
}
