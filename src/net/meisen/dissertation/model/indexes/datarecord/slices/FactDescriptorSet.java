package net.meisen.dissertation.model.indexes.datarecord.slices;

import java.lang.reflect.Array;
import java.util.Collection;
import java.util.HashSet;
import java.util.Iterator;
import java.util.Set;
import java.util.TreeSet;

import net.meisen.dissertation.model.descriptors.FactDescriptor;
import net.meisen.general.genmisc.types.Objects;

/**
 * A {@code SortedSet} of {@code FactDescriptors}. The sorting is done using the
 * value of the {@code fact}, whereby all record variant {@code FactDescriptors}
 * are smaller than invariant once - sorted by their identifier. The value and
 * record invariant {@code FactDescriptor} instances are sorted by their
 * invariant value.
 * 
 * @author pmeisen
 * 
 */
public class FactDescriptorSet implements Collection<FactDescriptor<?>> {
	private final Set<FactDescriptor<?>> variantSet;
	private final Set<FactDescriptor<?>> nanSet;
	private final TreeSet<FactDescriptor<?>> nonNanSet;

	/**
	 * Creates a new instance of a {@code DescriptorSet}.
	 */
	public FactDescriptorSet() {
		this.nonNanSet = new TreeSet<FactDescriptor<?>>();

		// there won't be hopefully that many
		this.variantSet = new HashSet<FactDescriptor<?>>(0, (float) 1.00);

		// typically there won't be any Double.NaN
		this.nanSet = new HashSet<FactDescriptor<?>>(0, (float) 1.00);
	}

	/**
	 * Adds the specified element to {@code this} if it is not already present.
	 * More formally, adds the specified element <tt>e</tt> to {@code this} if
	 * the set contains no element <tt>e2</tt> such that
	 * <tt>(e==null&nbsp;?&nbsp;e2==null&nbsp;:&nbsp;e.equals(e2))</tt>. If
	 * {@code this} already contains the element, the call leaves {@code this}
	 * unchanged and returns <tt>false</tt>. In combination with the restriction
	 * on constructors, this ensures that {@code this} never contains duplicate
	 * elements.
	 * 
	 * @param fact
	 *            the fact to be added
	 * 
	 * @return <tt>true</tt> if {@code this} did not already contain the
	 *         specified element
	 */
	public boolean add(final FactDescriptor<?> fact) {

		final boolean variant = fact.isVariant();
		final boolean nan = Double.isNaN(fact.getFact());

		boolean removed = false;
		if (!removed && !variant) {
			removed = this.variantSet.remove(fact);
		}
		if (!removed && !nan) {
			removed = this.nanSet.remove(fact);
		}
		if (!removed && (nan || variant)) {
			removed = this.nonNanSet.remove(fact);
		}

		if (variant) {
			return this.variantSet.add(fact);
		} else if (nan) {
			return this.nanSet.add(fact);
		} else {

			/*
			 * The comparing of the factDesc follows a variant/invariant,
			 * fact-based equality. Instead, the equality checks the model and
			 * identifier of the associated descriptor. Thus, the adding might
			 * add a duplicate, if we don't remove the duplicate ourself.
			 */
			final FactDescriptor<?> cur = this.nonNanSet.ceiling(fact);
			if (cur == null || !cur.equals(fact)) {
				return this.nonNanSet.add(fact);
			} else {

				// the objects are equal but modified
				if (!Objects.equals(cur.getFact(), fact.getFact())) {
					this.nonNanSet.remove(cur);
					return this.nonNanSet.add(fact);
				} else {
					return removed;
				}
			}
		}
	}

	/**
	 * Adds all of the elements in the specified collection to {@code this}.
	 * 
	 * @param facts
	 *            collection containing elements to be added to {@code this}
	 * 
	 * @return {@code true} if this set changed as a result of the call
	 */
	public boolean addAll(final Collection<? extends FactDescriptor<?>> facts) {
		boolean res = false;
		for (final FactDescriptor<?> fact : facts) {
			res = add(fact) && res;
		}

		return res;
	}

	/**
	 * Gets the amount of {@code Double.NaN} invariant {@code FactDescriptor}
	 * instances.
	 * 
	 * @return the amount of {@code Double.NaN} invariant {@code FactDescriptor}
	 *         instances
	 */
	public int nanSize() {
		return nanSet.size();
	}

	/**
	 * Gets the amount of non {@code Double.NaN} invariant
	 * {@code FactDescriptor} instances.
	 * 
	 * @return the amount of non {@code Double.NaN} invariant
	 *         {@code FactDescriptor} instances
	 */
	public int nonNanSize() {
		return this.nonNanSet.size();
	}

	/**
	 * Gets the amount of {variant {@code FactDescriptor} instances.
	 * 
	 * @return the amount of variant {@code FactDescriptor} instances
	 */
	public int variantSize() {
		return this.variantSet.size();
	}

	/**
	 * Iterator to iterate over the invariant values of the
	 * {@code FactDescriptorSet}. The iteration iterates over the sorted (by
	 * facts) {@code FactDescriptors} in ascending order, whereby the
	 * {@code Double.NaN} are appended to the end.
	 * 
	 * @return an iterator to iterate over the values in ascending order
	 */
	public Iterator<FactDescriptor<?>> iterator() {
		return iterator(false);
	}

	/**
	 * Iterator to iterate over the invariant values of the
	 * {@code FactDescriptorSet}. The iteration iterates over the sorted (by
	 * facts) {@code FactDescriptors} in descending order, whereby the
	 * {@code Double.NaN} are appended to the end.
	 * 
	 * @return an iterator to iterate over the values in descending order
	 */
	public Iterator<FactDescriptor<?>> descendingIterator() {
		return iterator(true);
	}

	/**
	 * Iterates over the invariant {@code FactDescriptor} instances of
	 * {@code this}.
	 * 
	 * @param descending
	 *            the order to iterate in, {@code Double.NaN} values are always
	 *            last
	 * 
	 * @return an iterator
	 */
	protected Iterator<FactDescriptor<?>> iterator(final boolean descending) {
		final Iterator<FactDescriptor<?>> it = descending ? descendingNonNanIterator()
				: nonNanIterator();
		final Iterator<FactDescriptor<?>> nanIt = nanIterator();

		return new Iterator<FactDescriptor<?>>() {

			@Override
			public boolean hasNext() {
				return it.hasNext() || nanIt.hasNext();
			}

			@Override
			public FactDescriptor<?> next() {
				if (it.hasNext()) {
					return it.next();
				} else if (nanIt.hasNext()) {
					return nanIt.next();
				} else {
					throw new IllegalStateException("There is no next element.");
				}
			}

			@Override
			public void remove() {
				throw new UnsupportedOperationException(
						"Remove is not supported.");
			}
		};
	}

	/**
	 * Iterator to iterate over the non {@code Double.NaN}
	 * {@code FactDescriptor} instances in descending order.
	 * 
	 * @return iterator to iterate over the non {@code Double.NaN}
	 */
	public Iterator<FactDescriptor<?>> descendingNonNanIterator() {
		return this.nonNanSet.descendingIterator();
	}

	/**
	 * Iterator to iterate over the non {@code Double.NaN}
	 * {@code FactDescriptor} instances in ascending order.
	 * 
	 * @return iterator to iterate over the non {@code Double.NaN}
	 */
	public Iterator<FactDescriptor<?>> nonNanIterator() {
		return this.nonNanSet.iterator();
	}

	/**
	 * Iterator to iterate over the {@code Double.NaN} {@code FactDescriptor}
	 * instances.
	 * 
	 * @return iterator to iterate over the {@code Double.NaN}
	 */
	public Iterator<FactDescriptor<?>> nanIterator() {
		return nanSet.iterator();
	}

	/**
	 * Iterator to iterate over the variant {@code FactDescriptor} instances.
	 * 
	 * @return iterator to iterate over the variant {@code FactDescriptor}
	 *         instances
	 */
	public Iterator<FactDescriptor<?>> variantIterator() {
		return variantSet.iterator();
	}

	/**
	 * The complete size of the set.
	 * 
	 * @return the complete size, i.e. all types are included
	 */
	public int size() {
		return nonNanSize() + nanSize() + variantSize();
	}

	/**
	 * Checks if the set contains a {@code Descriptor} which is record variant.
	 * 
	 * @return {@code true} if the set contains a record which is record variant
	 */
	public boolean containsVariantRecords() {
		return variantSize() > 0;
	}

	@Override
	public boolean isEmpty() {
		return size() == 0;
	}

	@Override
	public Object[] toArray() {
		return toArray(new Object[] {});
	}

	@Override
	public <T> T[] toArray(final T[] template) {
		final T[] nonNanArray = this.nonNanSet.toArray(template);
		final T[] nanArray = this.nanSet.toArray(template);
		final T[] variantArray = this.variantSet.toArray(template);

		final int nonNanLen = nonNanArray.length;
		final int nanLen = nanArray.length;
		final int variantNanLen = variantArray.length;

		@SuppressWarnings("unchecked")
		final T[] res = (T[]) Array.newInstance(template.getClass()
				.getComponentType(), nanLen + nonNanLen + variantNanLen);

		System.arraycopy(nonNanArray, 0, res, 0, nonNanLen);
		System.arraycopy(nanArray, 0, res, nonNanLen, nanLen);
		System.arraycopy(variantArray, 0, res, nanLen + nonNanLen,
				variantNanLen);

		return res;
	}

	@Override
	public boolean contains(final Object o) {
		return this.nanSet.contains(o) || this.variantSet.contains(o)
				|| this.nonNanSet.contains(o);
	}

	@Override
	public boolean remove(final Object o) {
		return this.nanSet.remove(o) || this.variantSet.remove(o)
				|| this.nonNanSet.remove(o);
	}

	@Override
	public boolean containsAll(final Collection<?> c) {
		for (final Object o : c) {
			if (!contains(o)) {
				return false;
			}
		}

		return true;
	}

	@Override
	public boolean removeAll(final Collection<?> c) {
		boolean change = false;

		for (final Object o : c) {
			change = remove(o) || change;
		}

		return change;
	}

	@Override
	public boolean retainAll(final Collection<?> c)
			throws UnsupportedOperationException {
		throw new UnsupportedOperationException("Retain is not supported");
	}

	@Override
	public void clear() {
		this.nanSet.clear();
		this.nonNanSet.clear();
		this.variantSet.clear();
	}

	@Override
	public boolean equals(final Object obj) {
		if (obj == this) {
			return true;
		} else if (obj instanceof FactDescriptorSet) {
			final FactDescriptorSet set = (FactDescriptorSet) obj;

			return set.variantSet.equals(variantSet)
					&& set.nanSet.equals(nanSet)
					&& set.nonNanSet.equals(nonNanSet);
		} else {
			return false;
		}
	}
}
