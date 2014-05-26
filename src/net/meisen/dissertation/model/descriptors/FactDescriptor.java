package net.meisen.dissertation.model.descriptors;

import net.meisen.general.genmisc.types.Objects;

public class FactDescriptor<I> {
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

			if (isRecordInvariant() == fd.isRecordInvariant()) {
				return Objects.equals(fd.getId(), getId())
						&& Objects.equals(fd.getModelId(), getModelId());
			} else {
				return false;
			}
		} else {
			return false;
		}
	}

	@Override
	public String toString() {
		return getModelId() + "." + getId() + " = " + getFact();
	}
}
