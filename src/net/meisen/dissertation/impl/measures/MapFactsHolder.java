package net.meisen.dissertation.impl.measures;

import net.meisen.dissertation.model.indexes.datarecord.TidaIndex;
import net.meisen.dissertation.model.indexes.datarecord.bitmap.Bitmap;
import net.meisen.dissertation.model.indexes.datarecord.slices.FactDescriptorSet;
import net.meisen.dissertation.model.measures.IFactsHolder;

public class MapFactsHolder implements IFactsHolder {
	private MapFactsArrayBased factsArray;

	@Override
	public void init(final int maxRecordId, int capacity) {
		factsArray = new MapFactsArrayBased(maxRecordId, capacity);
	}

	@Override
	public void init(final FactDescriptorSet descriptors,
			final TidaIndex index, final Bitmap bitmap) {
		factsArray = new MapFactsDescriptorBased(descriptors, index, bitmap);
	}

	@Override
	public void set(final int recordId, final double factValue) {
		factsArray.set(recordId, factValue);
	}

	@Override
	public double getFactOfRecord(final int recordId) {
		return factsArray.getFactOfRecord(recordId);
	}

	@Override
	public int[] recordIds() {
		return factsArray.recordIds();
	}

	@Override
	public double[] facts() {
		return factsArray.facts();
	}

	@Override
	public double[] sortedFacts() {
		return factsArray.sortedFacts();
	}

	@Override
	public int maxRecordId() {
		return factsArray.maxRecordId();
	}

	@Override
	public int amountOfFacts() {
		return factsArray.size();
	}
}
