package net.meisen.dissertation.impl.cache;

import java.io.DataInput;
import java.io.DataOutput;
import java.io.IOException;
import java.util.Set;

import net.meisen.dissertation.model.cache.IFactDescriptorModelSetCache;
import net.meisen.dissertation.model.cache.IFactDescriptorModelSetCacheConfig;
import net.meisen.dissertation.model.cache.IReleaseMechanismCache;
import net.meisen.dissertation.model.descriptors.FactDescriptor;
import net.meisen.dissertation.model.indexes.datarecord.slices.BitmapId;
import net.meisen.dissertation.model.indexes.datarecord.slices.FactDescriptorModelSet;
import net.meisen.dissertation.model.indexes.datarecord.slices.FactDescriptorSet;
import net.meisen.general.genmisc.resources.IByteBufferReader;
import net.meisen.general.genmisc.types.Streams;

/**
 * A file-based cache for {@code FactDescriptorModelSet} instances.
 * 
 * @author pmeisen
 * 
 */
public class FileFactDescriptorModelSetCache extends
		BaseFileBitmapIdCache<FactDescriptorModelSet> implements
		IFactDescriptorModelSetCache,
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

		// write the amount of models to be expected
		final Set<String> models = set.getModels();
		out.write(Streams.intToByte(models.size()));

		// write each model
		for (final String modelId : models) {
			final FactDescriptorSet facts = set.getDescriptors(modelId);

			// write the modelIdentifier
			out.write(Streams.objectToByte(modelId));
			out.write(Streams.intToByte(facts.size()));

			// add the different values
			for (final FactDescriptor<?> fact : facts) {
				out.write(Streams.doubleToByte(fact.getFact()));
				out.write(Streams.booleanToByte(fact.isRecordInvariant()));
				out.write(Streams.objectToByte(fact.getId()));
			}
		}
	}

	@SuppressWarnings({ "rawtypes", "unchecked" })
	@Override
	protected FactDescriptorModelSet createFromInput(final DataInput in)
			throws IOException {
		final IByteBufferReader reader = Streams.createByteBufferReader(in,
				1024);

		// create an empty set
		final FactDescriptorModelSet set = new FactDescriptorModelSet();

		// get the amount of models
		final int modelsSize = Streams.readNextObject(reader, Integer.class);

		// read each model
		for (int i = 0; i < modelsSize; i++) {
			final String descModelId = (String) Streams.readNextObject(reader);
			final int factsSize = Streams.readNextObject(reader, Integer.class);

			// read the facts of the model
			for (int k = 0; k < factsSize; k++) {
				final double fact = Streams
						.readNextObject(reader, Double.class);
				final boolean invariant = Streams.readNextObject(reader,
						Boolean.class);
				final Object descId = Streams.readNextObject(reader);

				// create the FactDescriptor
				final FactDescriptor des;
				if (invariant) {
					des = new FactDescriptor(descModelId, descId, fact);
				} else {
					des = new FactDescriptor(descModelId, descId);
				}

				// add the descriptor
				set.add(des);
			}
		}

		return set;
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
	public void setConfig(final IFactDescriptorModelSetCacheConfig configuration) {
		if (configuration == null
				|| configuration instanceof FileFactDescriptorModelSetCacheConfig) {
			super.setConfiguration((FileFactDescriptorModelSetCacheConfig) configuration);
		} else {
			exceptionRegistry.throwException(getExceptionClass(1001), 1001,
					configuration.getClass().getName());
		}
	}

	@Override
	public void cacheFactDescriptorModelSet(final BitmapId<?> bitmapId,
			final FactDescriptorModelSet set) {
		cache(bitmapId, set);
	}

	@Override
	public FactDescriptorModelSet get(final BitmapId<?> bitmapId) {
		return getCacheable(bitmapId);
	}
}