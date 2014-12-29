package net.meisen.dissertation.impl.parser.query.insert;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Iterator;
import java.util.List;

import net.meisen.dissertation.impl.datasets.SingleStaticDataSet;
import net.meisen.dissertation.impl.parser.query.Interval;
import net.meisen.dissertation.impl.parser.query.IntervalType;
import net.meisen.dissertation.jdbc.protocol.QueryType;
import net.meisen.dissertation.model.auth.IAuthManager;
import net.meisen.dissertation.model.auth.permissions.DefinedPermission;
import net.meisen.dissertation.model.auth.permissions.Permission;
import net.meisen.dissertation.model.data.DataStructure;
import net.meisen.dissertation.model.data.TidaModel;
import net.meisen.dissertation.model.datasets.IDataRecord;
import net.meisen.dissertation.model.datastructure.IntervalStructureEntry;
import net.meisen.dissertation.model.datastructure.MetaStructureEntry;
import net.meisen.dissertation.model.datastructure.StructureEntry;
import net.meisen.dissertation.model.handler.TidaModelHandler;
import net.meisen.dissertation.model.parser.query.IQuery;
import net.meisen.dissertation.model.parser.query.IQueryResult;
import net.meisen.dissertation.model.parser.query.IResourceResolver;

/**
 * A query used to insert values into a {@code TidaModel}.
 * 
 * @author pmeisen
 * 
 */
public class InsertQuery implements IQuery {

	private final List<List<String>> data;
	private final List<Interval<?>> intervals;

	private String modelId;

	private IntervalType startIntervalType;
	private IntervalType endIntervalType;

	private int startPosition;
	private int endPosition;

	private List<String> descIds;

	private boolean enableIdCollection;

	/**
	 * Default constructor.
	 */
	public InsertQuery() {
		this.data = new ArrayList<List<String>>();
		this.intervals = new ArrayList<Interval<?>>();
		this.descIds = Collections.emptyList();

		this.enableIdCollection(false);
	}

	/**
	 * Generates an iterator useful to iterate over the data of {@code this}.
	 * 
	 * @return an iterator to iterate over the data of {@code this}
	 */
	public Iterator<IDataRecord> it() {
		return new Iterator<IDataRecord>() {
			private int pos = 0;

			@Override
			public boolean hasNext() {
				return pos < data.size();
			}

			@Override
			public IDataRecord next() {
				if (!hasNext()) {
					throw new IllegalStateException(
							"The iterator has no more elements.");
				}

				// get the specified values
				final Interval<?> interval = intervals.get(pos);
				final List<String> descValues = data.get(pos);

				// increase the position
				pos++;

				final Object[] values = new Object[descValues.size() + 2];
				values[0] = interval.getStart();
				values[1] = interval.getEnd();
				for (int i = 0; i < descValues.size(); i++) {
					values[i + 2] = descValues.get(i);
				}

				return new SingleStaticDataSet(values);
			}

			@Override
			public void remove() {
				throw new UnsupportedOperationException(
						"Removing is not supported.");
			}
		};
	}

	/**
	 * Gets the {@code DataStructure} defined for {@code this}. The structure is
	 * created by every call of the method.
	 * 
	 * @return the {@code DataStructure} defined for {@code this}
	 * 
	 * @see DataStructure
	 */
	public DataStructure getDataStructure() {
		int nextPos = 1;
		final List<StructureEntry> structures = new ArrayList<StructureEntry>(
				sizeOfDescriptorModelIds() + 2);

		// add the interval-StructureEntries
		structures.add(new IntervalStructureEntry("START", startIntervalType
				.isInclusive(), nextPos++));
		structures.add(new IntervalStructureEntry("END", endIntervalType
				.isInclusive(), nextPos++));

		// add the meta-StructureEntries
		for (int i = 0; i < descIds.size(); i++) {
			final String descId = descIds.get(i);
			structures.add(new MetaStructureEntry(descId, nextPos++));
		}

		return new DataStructure(structures);
	}

	@Override
	public String getModelId() {
		return modelId;
	}

	@Override
	public void setModelId(final String modelId) {
		this.modelId = modelId;
	}

	@Override
	public IQueryResult evaluate(final IAuthManager authManager,
			final TidaModelHandler handler, final TidaModel model,
			final IResourceResolver resolver) {

		if (isEnableIdCollection()) {
			final int[] ids = model.bulkLoadDataWithIds(getDataStructure(),
					it());
			return new InsertResult(ids);
		} else {
			final int amount = model.bulkLoadData(getDataStructure(), it());
			return new InsertResult(amount);
		}
	}

	/**
	 * Sets the start position and the {@code IntervalType}.
	 * 
	 * @param position
	 *            the position within the query to read the start from
	 * @param intervalType
	 *            the type of the start
	 */
	public void setStart(final int position, final IntervalType intervalType) {
		this.startPosition = position;
		this.startIntervalType = intervalType;
	}

	/**
	 * Sets the end position and the {@code IntervalType}.
	 * 
	 * @param position
	 *            the position within the query to read the end from
	 * @param intervalType
	 *            the type of the end
	 */
	public void setEnd(final int position, final IntervalType intervalType) {
		this.endPosition = position;
		this.endIntervalType = intervalType;
	}

	/**
	 * Gets the defined identifier of the {@code DescriptorModel} instances.
	 * 
	 * @return the defined identifier of the {@code DescriptorModel} instances
	 */
	public List<String> getDescriptorModelIds() {
		return descIds;
	}

	/**
	 * Gets the defined {@code IntervalType} of the start.
	 * 
	 * @return the defined {@code IntervalType} of the start
	 */
	public IntervalType getStartIntervalType() {
		return startIntervalType;
	}

	/**
	 * Gets the defined {@code IntervalType} of the end.
	 * 
	 * @return the defined {@code IntervalType} of the end
	 */
	public IntervalType getEndIntervalType() {
		return endIntervalType;
	}

	/**
	 * Adds a record to {@code this}.
	 * 
	 * @param interval
	 *            the interval of the record
	 * @param data
	 *            the data, i.e. the values of the {@code Descriptors}
	 */
	public void addData(final Interval<?> interval, final List<String> data) {
		this.data.add(data);
		this.intervals.add(interval);
	}

	/**
	 * Sets the identifiers of the {@code DescriptorModel} instances. The order
	 * is important, because the order of the data must be as defined by
	 * {@code ids}.
	 * 
	 * @param ids
	 *            the identifiers of the {@code DescriptorModel} instances
	 */
	public void setDescriptorModels(final List<String> ids) {
		this.descIds = ids;
	}

	/**
	 * Gets the defined position of the start value within the query.
	 * 
	 * @return the defined position of the start value within the query
	 */
	public int getStartPosition() {
		return startPosition;
	}

	/**
	 * Gets the defined position of the end value within the query.
	 * 
	 * @return the defined position of the end value within the query
	 */
	public int getEndPosition() {
		return endPosition;
	}

	/**
	 * Gets the amount of {@code DescriptorModel} identifiers defined within the
	 * query.
	 * 
	 * @return the amount of {@code DescriptorModel} identifiers defined within
	 *         the query
	 */
	public int sizeOfDescriptorModelIds() {
		return descIds.size();
	}

	/**
	 * Gets the amount of records to be inserted.
	 * 
	 * @return the amount of records to be inserted
	 */
	public int sizeOfRecords() {
		assert data.size() == intervals.size();

		return data.size();
	}

	@Override
	public String toString() {
		return "(" + startIntervalType.toString(true) + " (" + startPosition
				+ "), " + endIntervalType.toString(false) + " (" + endPosition
				+ "), " + descIds + ")";
	}

	/**
	 * Gets the {@code Interval} for the specified zero-based {@code recordPos}.
	 * 
	 * @param recordPos
	 *            the zero-based position to get the {@code Interval} for
	 * 
	 * @return the {@code Interval} defined for the {@code recordPos}
	 */
	public Interval<?> getInterval(final int recordPos) {
		return intervals.get(recordPos);
	}

	/**
	 * Gets the values for the descriptors for the specified zero-based
	 * {@code recordPos}.
	 * 
	 * @param recordPos
	 *            the zero-based position to get the values for the descriptors
	 * 
	 * @return the values for the descriptors defined for the {@code recordPos}
	 */
	public List<String> getDescriptorValues(final int recordPos) {
		return data.get(recordPos);
	}

	@Override
	public boolean expectsModel() {
		return true;
	}

	@Override
	public QueryType getQueryType() {
		return QueryType.MANIPULATION;
	}

	@Override
	public void enableIdCollection(final boolean enableIdCollection) {
		this.enableIdCollection = enableIdCollection;
	}

	/**
	 * Checks if the collection of identifiers is enabled.
	 * 
	 * @return {@code true} if the collection is enabled, otherwise
	 *         {@code false}
	 */
	public boolean isEnableIdCollection() {
		return enableIdCollection;
	}

	@Override
	public DefinedPermission[][] getNeededPermissions() {
		return new DefinedPermission[][] {
				new DefinedPermission[] { Permission.modify.create(modelId) },
				new DefinedPermission[] { Permission.modifyAll.create() } };
	}
}
