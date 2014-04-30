package net.meisen.dissertation.model.indexes.datarecord.slices;

import java.util.Comparator;
import java.util.TreeSet;

import net.meisen.dissertation.model.descriptors.Descriptor;
import net.meisen.general.genmisc.types.Objects;

/**
 * A {@code SortedSet} of {@code Descriptors}. The sorting is done using the
 * value of the {@code descriptor}, whereby all record variant
 * {@code Descriptors} are smaller than invariant once - sorted by their
 * identifier. The record invariant {@code Descriptors} are sorted by their
 * invariant value.
 * 
 * @author pmeisen
 * 
 */
public class FactDescriptorSet extends TreeSet<Descriptor<?, ?, ?>> {
	private static final long serialVersionUID = 4576995616789676834L;

	private final static Comparator<Descriptor<?, ?, ?>> valueComperator = new Comparator<Descriptor<?, ?, ?>>() {

		/**
		 * Compares the two {@code Descriptor} instances. If one of the
		 * descriptors isn't record invariant an exception is thrown.
		 * 
		 * @param desc1
		 *            the first {@code Descriptor} to compare
		 * @param desc2
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
		public int compare(final Descriptor<?, ?, ?> desc1,
				final Descriptor<?, ?, ?> desc2) {

			if (desc1 == null || desc2 == null) {
				throw new NullPointerException(
						"Null descriptors are not supported!");
			}

			// make sure both are invariant
			final boolean invariantDesc1 = desc1.isRecordInvariant();
			final boolean invariantDesc2 = desc2.isRecordInvariant();
			if (invariantDesc1 && invariantDesc2) {
				final double factDesc1 = desc1.getFactValue(null);
				final double factDesc2 = desc2.getFactValue(null);

				if (factDesc1 < factDesc2) {
					return -1;
				} else if (factDesc1 > factDesc2) {
					return 1;
				}

				/*
				 * the models are equal within one set, which is ensured by the
				 * implementation, therefore just check the identifiers
				 */
				return Objects.compare(desc1.getId(), desc2.getId());
			}
			// both are variant
			else if (!invariantDesc1 && !invariantDesc2) {
				return Objects.compare(desc1.getId(), desc2.getId());
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
	protected Comparator<Descriptor<?, ?, ?>> getValueComparator() {
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
