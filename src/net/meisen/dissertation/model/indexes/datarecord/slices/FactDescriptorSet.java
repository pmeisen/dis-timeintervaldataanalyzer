package net.meisen.dissertation.model.indexes.datarecord.slices;

import java.util.Comparator;
import java.util.TreeSet;

import net.meisen.dissertation.model.descriptors.Descriptor;
import net.meisen.dissertation.model.descriptors.FactDescriptor;
import net.meisen.general.genmisc.types.Objects;

/**
 * A {@code SortedSet} of {@code FactDescriptors}. The sorting is done using the
 * value of the {@code fact}, whereby all record variant {@code FactDescriptors}
 * are smaller than invariant once - sorted by their identifier. The record
 * invariant {@code FactDescriptor} are sorted by their invariant value.
 * 
 * @author pmeisen
 * 
 */
public class FactDescriptorSet extends TreeSet<FactDescriptor<?>> {
	private static final long serialVersionUID = 4576995616789676834L;

	private final static Comparator<FactDescriptor<?>> valueComperator = new Comparator<FactDescriptor<?>>() {

		/**
		 * Compares the two {@code Descriptor} instances. If one of the
		 * descriptors isn't record invariant an exception is thrown.
		 * 
		 * @param factDesc1
		 *            the first {@code Descriptor} to compare
		 * @param factDesc2
		 *            the second {@code Descriptor} to compare
		 * 
		 * @return {@code 0} if both {@code Descriptor} instances are equal
		 *         considering their fact-values, {@code -1} if the fact-value
		 *         of the first {@code Descriptor} is smaller than the
		 *         fact-value of the second, and {@code 1} if the fact-value of
		 *         the first {@code Descriptor} is larger than the one of the
		 *         second value
		 * 
		 * @throws IllegalStateException
		 *             if one of the {@code Descriptor} instances isn't record
		 *             invariant (see {@link Descriptor#isRecordInvariant()}
		 * 
		 * @see Descriptor
		 */
		@Override
		public int compare(final FactDescriptor<?> factDesc1,
				final FactDescriptor<?> factDesc2) {

			if (factDesc1 == null || factDesc2 == null) {
				throw new NullPointerException(
						"Null descriptors are not supported!");
			}

			// make sure both are invariant
			final boolean invariantDesc1 = factDesc1.isRecordInvariant();
			final boolean invariantDesc2 = factDesc2.isRecordInvariant();
			if (invariantDesc1 && invariantDesc2) {
				final double fact1 = factDesc1.getFact();
				final double fact2 = factDesc2.getFact();

				if (fact1 < fact2) {
					return -1;
				} else if (fact1 > fact2) {
					return 1;
				}

				/*
				 * the models are equal within one set, which is ensured by the
				 * implementation, therefore just check the identifiers
				 */
				return Objects.compare(factDesc1.getId(), factDesc2.getId());
			}
			// both are variant
			else if (!invariantDesc1 && !invariantDesc2) {
				return Objects.compare(factDesc1.getId(), factDesc2.getId());
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
	};

	/**
	 * Creates a new instance of a {@code DescriptorSet}.
	 */
	public FactDescriptorSet() {
		super(valueComperator);
	}

	/**
	 * Gets a {@code Comparator} used to compare the values of instances of
	 * {@code Descriptor} instances. <br/>
	 * <br/>
	 * <b>Note:</b><br/>
	 * The {@code Descriptor} instances have sort the records by their values.
	 * If the {@code Descriptor} is record variant, i.e.
	 * {@link Descriptor#isRecordInvariant()} returns {@code false}, it is added
	 * first.
	 * 
	 * @return the {@code Comparator} to be used
	 * 
	 * @see Comparator
	 */
	protected Comparator<FactDescriptor<?>> getValueComparator() {
		return valueComperator;
	}

	/**
	 * Checks if the set contains a {@code Descriptor} which is record variant.
	 * 
	 * @return {@code true} if the set contains a record which is record variant
	 */
	public boolean containsVariantRecords() {
		return size() > 0 && !first().isRecordInvariant();
	}
}
