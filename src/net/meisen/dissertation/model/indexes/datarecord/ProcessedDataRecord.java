package net.meisen.dissertation.model.indexes.datarecord;

import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import net.meisen.dissertation.model.data.DataStructure;
import net.meisen.dissertation.model.data.IntervalModel;
import net.meisen.dissertation.model.data.IntervalModel.MappingResult;
import net.meisen.dissertation.model.data.MetaDataModel;
import net.meisen.dissertation.model.data.TidaModel;
import net.meisen.dissertation.model.datasets.IDataRecord;
import net.meisen.dissertation.model.datastructure.IntervalStructureEntry;
import net.meisen.dissertation.model.datastructure.MetaStructureEntry;
import net.meisen.dissertation.model.datastructure.StructureEntry;
import net.meisen.dissertation.model.descriptors.Descriptor;
import net.meisen.dissertation.model.descriptors.DescriptorModel;

public class ProcessedDataRecord {
	private final IDataRecord raw;

	private final Map<MetaStructureEntry, Descriptor<?, ?, ?>> processedMeta;

	private long start = -1;
	private long end = -1;

	public ProcessedDataRecord(final IDataRecord raw, final TidaModel model) {
		this.raw = raw;
		this.processedMeta = new HashMap<MetaStructureEntry, Descriptor<?, ?, ?>>();

		// iterate over the different entries and add the values
		final DataStructure dataStructure = model.getDataStructure();

		// handle the IntervalStructureEntries
		final List<IntervalStructureEntry> intervalEntries = dataStructure
				.getEntriesByClass(IntervalStructureEntry.class);
		IntervalStructureEntry start = null, end = null;
		for (final IntervalStructureEntry entry : intervalEntries) {
			if (entry.isStart()) {
				start = entry;
			} else {
				end = entry;
			}
		}
		setIntervalEntries(start, end, model);

		// handle the MetaStructureEntries
		final List<MetaStructureEntry> metaEntries = dataStructure
				.getEntriesByClass(MetaStructureEntry.class);
		for (final MetaStructureEntry entry : metaEntries) {
			addDescriptor(entry, model);
		}
	}

	public Collection<Descriptor<?, ?, ?>> getAllDescriptors() {
		return processedMeta.values();
	}

	public Descriptor<?, ?, ?> getDescriptor(final MetaStructureEntry entry) {
		return processedMeta.get(entry);
	}

	public long getStart() {
		return start;
	}

	public long getEnd() {
		return end;
	}

	public long getIntervalValue(final IntervalStructureEntry entry) {
		if (entry.isStart()) {
			return start;
		} else {
			return end;
		}
	}

	public IDataRecord getRawRecord() {
		return raw;
	}

	@Override
	public String toString() {
		return "[" + start + ", " + end + "] : " + processedMeta + " : "
				+ " (record: " + raw + ")";
	}

	/**
	 * Adds the {@code Descriptor} of the {@code DataRecord} to be handled by
	 * the specified {@code MetaStructureEntry}.
	 * 
	 * @param metaEntry
	 *            the {@code MetaStructureEntry} to determine the
	 *            {@code Descriptor} for
	 * @param model
	 *            the {@code TidaModel}
	 */
	protected void addDescriptor(final MetaStructureEntry metaEntry,
			final TidaModel model) {
		final MetaDataModel metaDataModel = model.getMetaDataModel();
		if (metaDataModel == null) {
			return;
		}

		final Object value = getValue(raw, metaEntry);

		// get the DescriptorModel
		final DescriptorModel<?> descModel = metaDataModel
				.getDescriptorModel(metaEntry.getDescriptorModel());

		// get the Descriptor and add it
		final MetaDataHandling handling = model.getMetaDataHandling();
		final Descriptor<?, ?, ?> desc = descModel.getDescriptorByValue(value,
				handling);
		processedMeta.put(metaEntry, desc);
	}

	/**
	 * Determines the value for the specified {@code IntervalStructureEntry}.
	 * 
	 * @param startEntry
	 *            the {@code IntervalStructureEntry} used for the start
	 * @param endEntry
	 *            the {@code IntervalStructureEntry} used for the end
	 * @param model
	 *            the {@code TidaModel}
	 */
	protected void setIntervalEntries(final IntervalStructureEntry startEntry,
			final IntervalStructureEntry endEntry, final TidaModel model) {
		final IntervalModel intervalModel = model.getIntervalModel();
		if (intervalModel == null) {
			return;
		}

		// map the values
		final Object start = startEntry == null ? null : getValue(raw,
				startEntry);
		final Object end = endEntry == null ? null : getValue(raw, endEntry);
		final IntervalDataHandling handling = model.getIntervalDataHandling();
		final MappingResult res = intervalModel.mapToTimeline(start, end,
				handling);

		// add the results
		this.start = res.getStart();
		this.end = res.getEnd();
	}

	/**
	 * Gets the value at the specified position or the specified name of the
	 * {@code StructureEntry}.
	 * 
	 * @param record
	 *            the record to get the value from
	 * @param entry
	 *            the entry to get the value for
	 * 
	 * @return the determined value
	 */
	protected Object getValue(final IDataRecord record,
			final StructureEntry entry) {
		final String name = entry.getName();

		if (name == null) {
			return record.getValue(entry.getPosition());
		} else {
			return record.getValue(name);
		}
	}
}
