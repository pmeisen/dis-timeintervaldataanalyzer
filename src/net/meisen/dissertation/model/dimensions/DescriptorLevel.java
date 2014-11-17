package net.meisen.dissertation.model.dimensions;

import net.meisen.dissertation.exceptions.DescriptorDimensionException;
import net.meisen.general.genmisc.exceptions.ForwardedRuntimeException;
import net.meisen.general.genmisc.types.Objects;

public class DescriptorLevel {

	private final String id;
	private final DescriptorHierarchy owner;

	private String name;

	public DescriptorLevel(final String id, final DescriptorHierarchy owner) {
		this(id, id, owner);
	}

	public DescriptorLevel(final String id, final String name,
			final DescriptorHierarchy owner) {
		if (owner == null) {
			throw new ForwardedRuntimeException(
					DescriptorDimensionException.class, 1007, id);
		}

		this.id = id;
		this.name = name;
		this.owner = owner;
	}

	public String getId() {
		return id;
	}

	public String getName() {
		return name;
	}

	public void setName(final String name) {
		this.name = name;
	}

	public DescriptorHierarchy getHierachy() {
		return owner;
	}

	@Override
	public int hashCode() {
		return id.hashCode();
	}

	@Override
	public boolean equals(final Object obj) {
		if (obj == this) {
			return true;
		} else if (obj == null) {
			return false;
		} else if (obj instanceof DescriptorLevel) {
			final DescriptorLevel cmpLevel = (DescriptorLevel) obj;
			return Objects.equals(id, cmpLevel.getId())
					&& Objects.equals(getHierachy(), cmpLevel.getHierachy());
		} else {
			return false;
		}
	}

	@Override
	public String toString() {
		return "[" + owner.getId() + "]: " + getName() + " (" + getId() + ")";
	}
}
