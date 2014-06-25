package net.meisen.dissertation.model.indexes.datarecord;

import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

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

/**
 * A processed {@code DataRecord} is a record which is processed by a
 * {@code TidaModel} definition.
 * 
 * @author pmeisen
 * 
 */
public class ProcessedDataRecord {
	private final static Logger LOG = LoggerFactory
			.getLogger(ProcessedDataRecord.class);

	private final IDataRecord raw;
	private final Map<String, Descriptor<?, ?, ?>> processedMeta;
	private final int id;

	private long start = -1;
	private long end = -1;

	/**
	 * Constructor to create a {@code ProcessedDataRecord} based on the
	 * {@code TidaModel} and the {@code DataRecord}.
	 * 
	 * @param raw
	 *            the {@code DataRecord} to be processed
	 * @param model
	 *            the {@code Model} which defines how to process the record
	 * @param id
	 *            the identifier of the record
	 */
	public ProcessedDataRecord(final IDataRecord raw, final TidaModel model,
			final int id) {
		this(model.getDataStructure(), raw, model, id);
	}

	/**
	 * Constructor to create a {@code ProcessedDataRecord} based on the
	 * {@code TidaModel} and the {@code DataRecord}.
	 * 
	 * @param dataStructure
	 *            overrides the {@code DataStructure} of the specified
	 *            {@code model}
	 * @param raw
	 *            the {@code DataRecord} to be processed
	 * @param model
	 *            the {@code Model} which defines how to process the record
	 * @param id
	 *            the identifier of the record
	 */
	public ProcessedDataRecord(final DataStructure dataStructure,
			final IDataRecord raw, final TidaModel model, final int id) {
		this.id = id;
		this.raw = raw;
		this.processedMeta = new HashMap<String, Descriptor<?, ?, ?>>();

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

		if (LOG.isTraceEnabled()) {
			LOG.trace("Processing of record finished: " + this);
		}
	}

	/**
	 * Gets all the {@code Descriptors} addressed by {@code this} record.
	 * 
	 * @return all the {@code Descriptors} addressed by {@code this} record
	 */
	public Collection<Descriptor<?, ?, ?>> getAllDescriptors() {
		return processedMeta.values();
	}

	/**
	 * Gets the {@code Descriptor} of the specified {@code entry}.
	 * 
	 * @param entry
	 *            the {@code MetaStructureEntry} to get the {@code Descriptor}
	 *            for
	 * 
	 * @return the {@code Descriptor} for the specified {@code entry}
	 */
	public Descriptor<?, ?, ?> getDescriptor(final MetaStructureEntry entry) {
		return processedMeta.get(entry.getDescriptorModel());
	}

	/**
	 * Get the {@code Descriptor} of the specified {@code model}.
	 * 
	 * @param model
	 *            the {@code DescriptorModel} to get the {@code Descriptor} for
	 * 
	 * @return the {@code Descriptor} for the specified {@code model}
	 */
	public Descriptor<?, ?, ?> getDescriptor(final DescriptorModel<?> model) {
		return getDescriptor(model.getId());
	}

	/**
	 * Get the {@code Descriptor} of the specified {@code modelId}.
	 * 
	 * @param modelId
	 *            the identifier of the {@code DescriptorModel} to get the
	 *            {@code Descriptor} for
	 * 
	 * @return the {@code Descriptor} for the specified {@code model}
	 */
	public Descriptor<?, ?, ?> getDescriptor(final String modelId) {
		return processedMeta.get(modelId);
	}

	/**
	 * Gets the mapped, normalized start value on the timeline of the records
	 * interval.
	 * 
	 * @return the mapped, normalized start value
	 */
	public long getStart() {
		return start;
	}

	/**
	 * Gets the mapped, normalized end value on the timeline of the records
	 * interval.
	 * 
	 * @return the mapped, normalized end value
	 */
	public long getEnd() {
		return end;
	}

	/**
	 * Gets the interval value for the specified {@code entry}.
	 * 
	 * @param entry
	 *            the entry to get the mapped, normalized value for
	 * 
	 * @return the normalized, mapped value for the specified {@code entry}
	 */
	public long getIntervalValue(final IntervalStructureEntry entry) {
		if (entry.isStart()) {
			return start;
		} else {
			return end;
		}
	}

	/**
	 * Get the raw record used to determine the values from.
	 * 
	 * @return the raw record used to determine the values from
	 */
	public IDataRecord getRawRecord() {
		return raw;
	}

	@Override
	public String toString() {
		return id + " : [" + start + ", " + end + "] : " + processedMeta
				+ " : " + " (record: " + raw + ")";
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
		processedMeta.put(metaEntry.getDescriptorModel(), desc);
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
		} else if (startEntry == null || endEntry == null) {
			this.start = -1;
			this.end = -1;
		} else {

			// map the values
			final boolean startInclusive = startEntry.isInclusive();
			final boolean endInclusive = endEntry.isInclusive();
			final Object start = getValue(raw, startEntry);
			final Object end = getValue(raw, endEntry);
			final IntervalDataHandling handling = model
					.getIntervalDataHandling();

			final MappingResult res = intervalModel.mapToTimeline(start, end,
					handling, startInclusive, endInclusive);

			// add the results
			this.start = res.getStart();
			this.end = res.getEnd();
		}
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

	/**
	 * Gets the identifier assigned to the record.
	 * 
	 * @return the identifier assigned to the record
	 */
	public int getId() {
		return id;
	}
}
