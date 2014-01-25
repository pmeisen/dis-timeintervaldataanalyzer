package net.meisen.dissertation.model.indexes.keys;

import net.meisen.dissertation.model.indexes.IndexKey;
import net.meisen.general.genmisc.types.Objects;

/**
 * Implementation of a {@code IndexKey} which wraps an {@code Object}.
 * 
 * @author pmeisen
 * 
 */
public class WrappedObjectIndexKey extends IndexKey<WrappedObjectIndexKey> {

	private Object object;

	/**
	 * Constructor which wraps the specified {@code object}.
	 * 
	 * @param object
	 *            the object to be wrapped
	 */
	public WrappedObjectIndexKey(final Object object) {
		this.object = object;
	}

	/**
	 * Gets the object which is wrapped by {@code this}.
	 * 
	 * @return the object which is wrapped by {@code this}
	 */
	public Object getWrappedObject() {
		return object;
	}

	@Override
	public int compareTo(final WrappedObjectIndexKey o) {
		if (this == o) {
			return 0;
		} else if (o == null) {
			return 1;
		} else {
			return compareObjects(this.object, o.object);
		}
	}

	@Override
	public boolean equals(final Object o) {
		if (this == o) {
			return true;
		} else if (o instanceof WrappedObjectIndexKey) {
			return Objects.equals(object, ((WrappedObjectIndexKey) o).object);
		} else {
			return false;
		}
	}

	@Override
	public int hashCode() {
		return object == null ? 0 : object.hashCode();
	}

	@Override
	public String toString() {
		return this.getClass().getName() + " (" + object + ")";
	}

	@Override
	public Object[] getValues() {
		return new Object[] { object };
	}
}
