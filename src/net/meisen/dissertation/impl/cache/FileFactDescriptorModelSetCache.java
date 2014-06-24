package net.meisen.dissertation.impl.cache;

import java.io.DataInput;
import java.io.DataOutput;
import java.io.IOException;

import net.meisen.dissertation.model.cache.IBitmapIdCache;
import net.meisen.dissertation.model.cache.IReleaseMechanismCache;
import net.meisen.dissertation.model.indexes.datarecord.slices.BitmapId;
import net.meisen.dissertation.model.indexes.datarecord.slices.FactDescriptorModelSet;

/**
 * A file-based cache for {@code FactDescriptorModelSet} instances.
 * 
 * @author pmeisen
 * 
 */
public class FileFactDescriptorModelSetCache extends
		BaseFileBitmapIdCache<FactDescriptorModelSet> implements
		IBitmapIdCache<FactDescriptorModelSet>,
		IReleaseMechanismCache<BitmapId<?>, FactDescriptorModelSet> {

	/**
	 * The name of the file used as index-table.
	 */
	protected final static String idxTableFileName = "fact.idx";
	/**
	 * The name of the file used to store the {@code FactDescriptorModelSet}.
	 */
	protected final static String fdmsFileName = "fact_"
			+ BaseFileBitmapIdCache.NR_MARKER + ".data";

	@Override
	protected void writeToOutput(final FactDescriptorModelSet set,
			final DataOutput out) throws IOException {
		set.serialize(out);
	}

	@Override
	protected FactDescriptorModelSet createFromInput(final DataInput in)
			throws IOException {
		return createNewInstance().deserialize(in);
	}

	@Override
	protected FactDescriptorModelSet createNewInstance() {
		return new FactDescriptorModelSet();
	}

	@Override
	protected String getIndexFileName() {
		return idxTableFileName;
	}

	@Override
	protected String getFileNamePattern() {
		return fdmsFileName;
	}

	@Override
	public FactDescriptorModelSet get(final BitmapId<?> bitmapId) {
		return getCacheable(bitmapId);
	}
}