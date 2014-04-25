package net.meisen.dissertation.impl.indexes;

import java.util.Arrays;
import java.util.Collection;

import net.meisen.dissertation.model.indexes.BaseIndexedCollection;
import net.meisen.dissertation.model.indexes.IndexKeyDefinition;

/**
 * An implementation of a {@code BaseIndexedCollection} based on a
 * {@code TIntObjectHashMap}.
 * 
 * @author pmeisen
 * 
 */
public class ContinuousIntIndexedCollection extends BaseIndexedCollection {
	private Object[] arrayList;
	private int offset = 0;

	/**
	 * Constructor which specifies the index's key.
	 * 
	 * @param keyDefinition
	 *            the {@code IndexKeyDefinition} to be used
	 */
	public ContinuousIntIndexedCollection(final IndexKeyDefinition keyDefinition) {
		super(keyDefinition);

		if (!keyDefinition.isSingleTypedKey(Integer.class)) {
			throw new IllegalArgumentException(
					"The key must be a single Integer");
		}

		// make sure we have an empty list in the beginning
		removeAll();
	}

	@Override
	public boolean containsObject(final Object object) {
		final int key = getKeyDefinition().getIntKey(object);
		return containsKey(key);
	}

	/**
	 * Checks if the specified key is within the index.
	 * 
	 * @param key
	 *            the key to be checked
	 * 
	 * @return {@code true} if it's an indexed key, otherwise {@code false}
	 */
	protected boolean containsKey(final int key) {
		if (key > offset - 1 && key < size() + offset) {
			return true;
		} else {
			return false;
		}
	}

	@Override
	public boolean addObject(final Object object) {
		final int key = getKeyDefinition().getIntKey(object);

		final boolean addToTail;
		if (containsKey(key)) {
			return false;
		}
		/*
		 * if there isn't any element yet, we use the first key as offset, a
		 * sorted, continuous ordering is needed for that to work
		 */
		else if (size() == 0) {
			offset = key;
			addToTail = true;
		}
		/*
		 * if we add an element prior to the first one, that is possible but it
		 * has to be directly prior to the first one
		 */
		else if (offset != 0 && key == offset - 1) {
			offset = offset - 1;
			addToTail = false;
		}
		/*
		 * check if the element is added to the tail
		 */
		else if (size() + offset == key) {
			addToTail = true;
		}
		/*
		 * otherwise the adding is invalid, i.e. not continuous
		 */
		else {
			return false;
		}

		// add the element
		if (addToTail) {
			arrayList = Arrays.copyOf(arrayList, arrayList.length + 1);
			arrayList[key - offset] = object;
		} else {

			final Object[] tmp = new Object[size() + 1];
			System.arraycopy(arrayList, 0, tmp, 1, arrayList.length);
			tmp[0] = object;
			arrayList = tmp;
		}

		return true;
	}

	@Override
	public void removeObject(final Object object) {
		final int key = getKeyDefinition().getIntKey(object);

		if (!containsKey(key)) {
			// nothing to do
		} else if (key == offset) {
			arrayList = Arrays.copyOfRange(arrayList, 1, arrayList.length);
			offset = offset + 1;
		} else if (key == offset + arrayList.length - 1) {
			arrayList = Arrays.copyOf(arrayList, arrayList.length - 1);
		} else {
			throw new IllegalArgumentException(
					"Cannot remove elements (i.e. "
							+ key
							+ ") from the middle of the index. Elements can only be removed from the tail or the head of the index.");
		}
	}

	@Override
	public void removeAll() {
		offset = 0;
		arrayList = new Object[0];
	}

	@Override
	public Object getObject(final Object... values) {
		if (values == null || values.length != 1) {
			throw new IllegalArgumentException(
					"The values cannot be null nor can it be multiple values.");
		}

		final int key = getKeyDefinition().generateIntKeyFromValue(values[0]);
		if (containsKey(key)) {
			return arrayList[key - offset];
		} else {
			return null;
		}
	}

	@Override
	public Collection<Object> getAll() {
		return Arrays.asList(arrayList);
	}

	@Override
	public int size() {
		return arrayList.length;
	}

	@Override
	public String toString() {
		return Arrays.asList(arrayList).toString();
	}

	/**
	 * Gets the current offset.
	 * 
	 * @return the offset
	 */
	protected int getOffset() {
		return offset;
	}
}
