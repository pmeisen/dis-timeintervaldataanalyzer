package net.meisen.dissertation.model.dimensions;

import net.meisen.dissertation.exceptions.DescriptorDimensionException;
import net.meisen.general.genmisc.exceptions.ForwardedRuntimeException;
import net.meisen.general.genmisc.types.Objects;

/**
 * A level defined for a {@code DescriptorDimension}.
 * 
 * @author pmeisen
 * 
 */
public class DescriptorLevel {

	private final String id;
	private final DescriptorHierarchy owner;

	private String name;

	/**
	 * Constructor specifying the {@code id} and the {@code owner} of the level.
	 * 
	 * @param id
	 *            the identifier
	 * @param owner
	 *            the owning {@code DescriptorHierarchy}
	 */
	public DescriptorLevel(final String id, final DescriptorHierarchy owner) {
		this(id, id, owner);
	}

	/**
	 * Constructor specifying the {@code id} and the {@code owner} of the level.
	 * 
	 * @param id
	 *            the identifier
	 * @param name
	 *            the name of the level
	 * @param owner
	 *            the owning {@code DescriptorHierarchy}
	 */
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

	/**
	 * Gets the identifier of the level.
	 * 
	 * @return the identifier of the level
	 */
	public String getId() {
		return id;
	}

	/**
	 * Gets the name of the level.
	 * 
	 * @return the name of the level
	 */
	public String getName() {
		return name;
	}

	/**
	 * Sets the name of the level.
	 * 
	 * @param name
	 *            the name of the level
	 */
	public void setName(final String name) {
		this.name = name;
	}

	/**
	 * Gets the hierarchy {@code this} belongs to.
	 * 
	 * @return the hierarchy {@code this} belongs to
	 */
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
