package net.meisen.dissertation.model.measures;

import net.meisen.dissertation.model.indexes.datarecord.TidaIndex;
import net.meisen.dissertation.model.indexes.datarecord.slices.Bitmap;
import net.meisen.dissertation.model.indexes.datarecord.slices.FactDescriptorSet;

public interface IFactsHolder {

	public void init(final int maxRecordId, final int capacity);

	public void init(final FactDescriptorSet descriptors,
			final TidaIndex index, final Bitmap bitmap);

	public double getFactOfRecord(final int recordId);

	public void set(final int recordId, final double factValue);

	public int maxRecordId();

	public int amountOfFacts();

	public int[] recordIds();

	public double[] facts();

	public double[] sortedFacts();
}
