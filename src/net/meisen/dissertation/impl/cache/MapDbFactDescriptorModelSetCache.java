package net.meisen.dissertation.impl.cache;

import net.meisen.dissertation.model.indexes.datarecord.slices.FactDescriptorModelSet;

/**
 * A cache used to store {@link FactDescriptorModelSet} instances based on a
 * {@code mapDb}.
 * 
 * @author pmeisen
 * 
 */
public class MapDbFactDescriptorModelSetCache extends
		BaseMapDbBitmapIdCache<FactDescriptorModelSet> {

	@Override
	protected String getIndexFileName() {
		return "mapDbFactDescriptorModelSet.idx";
	}

	@Override
	protected FactDescriptorModelSet createNewInstance() {
		return new FactDescriptorModelSet();
	}

	@Override
	protected MapDbFactDescriptorModelSetSerializer createValueSerializer() {
		return new MapDbFactDescriptorModelSetSerializer();
	}
}
