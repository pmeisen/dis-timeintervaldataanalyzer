package net.meisen.dissertation.model.indexes.datarecord.slices;

import java.util.TreeSet;

import net.meisen.dissertation.model.descriptors.FactDescriptor;

/**
 * A {@code SortedSet} of {@code FactDescriptors}. The sorting is done using the
 * value of the {@code fact}, whereby all record variant {@code FactDescriptors}
 * are smaller than invariant once - sorted by their identifier. The record
 * invariant {@code FactDescriptor} instances are sorted by their invariant value.
 * 
 * @author pmeisen
 * 
 */
public class FactDescriptorSet extends TreeSet<FactDescriptor<?>> {
	private static final long serialVersionUID = 4576995616789676834L;

	/**
	 * Creates a new instance of a {@code DescriptorSet}.
	 */
	public FactDescriptorSet() {
		super();
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
