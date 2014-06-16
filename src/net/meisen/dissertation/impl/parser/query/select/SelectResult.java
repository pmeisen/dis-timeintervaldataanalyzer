package net.meisen.dissertation.impl.parser.query.select;

import java.util.Arrays;
import java.util.Iterator;

import net.meisen.dissertation.impl.parser.query.select.evaluator.DescriptorLogicResult;
import net.meisen.dissertation.impl.parser.query.select.evaluator.GroupResult;
import net.meisen.dissertation.impl.time.series.TimeSeries;
import net.meisen.dissertation.impl.time.series.TimeSeriesCollection;
import net.meisen.dissertation.model.indexes.datarecord.slices.Bitmap;
import net.meisen.dissertation.model.parser.query.IQueryResultSet;

/**
 * The result of the evaluation of a {@code SelectQuery}.
 * 
 * @see SelectQuery
 * 
 * @author pmeisen
 * 
 */
public class SelectResult implements IQueryResultSet, Iterable<Object[]> {
	private final SelectQuery query;

	private Bitmap validRecords;
	private DescriptorLogicResult filterResult;
	private GroupResult groupResult;
	private TimeSeriesCollection timeSeriesCollection;

	/**
	 * Standard constructor which is used to specify the {@code query} this
	 * result is valid for.
	 * 
	 * @param query
	 *            the {@code SelectQuery} this result is valid for
	 */
	public SelectResult(final SelectQuery query) {
		this.query = query;
	}

	/**
	 * Gets the query.
	 * 
	 * @return the query
	 */
	public SelectQuery getQuery() {
		return query;
	}

	/**
	 * Gets the result of the filter specified by the query.
	 * 
	 * @return the result of the filter specified by the query
	 */
	public DescriptorLogicResult getFilterResult() {
		return filterResult;
	}

	/**
	 * Sets the result of the filter specified by the query.
	 * 
	 * @param filterResult
	 *            the result of the filtering
	 */
	public void setFilterResult(final DescriptorLogicResult filterResult) {
		this.filterResult = filterResult;
	}

	/**
	 * Sets the {@code TimeSeries} specified by the query.
	 * 
	 * @param timeSeriesCollection
	 *            the {@code TimeSeries} specified by the query
	 */
	public void setTimeSeriesResult(
			final TimeSeriesCollection timeSeriesCollection) {
		this.timeSeriesCollection = timeSeriesCollection;
	}

	/**
	 * Gets the determined {@code TimeSeries} specified by the query.
	 * 
	 * @return the determined {@code TimeSeries}
	 */
	public TimeSeriesCollection getTimeSeriesResult() {
		return timeSeriesCollection;
	}

	/**
	 * Gets the result of the group defined by the query.
	 * 
	 * @return the result of the group
	 */
	public GroupResult getGroupResult() {
		return groupResult;
	}

	/**
	 * Sets the result of the group.
	 * 
	 * @param groupResult
	 *            the grouping result
	 */
	public void setGroupResult(final GroupResult groupResult) {
		this.groupResult = groupResult;
	}

	/**
	 * Gets the valid records.
	 * 
	 * @return the valid records
	 */
	public Bitmap getValidRecords() {
		return validRecords;
	}

	/**
	 * Sets the valid records.
	 * 
	 * @param validRecords
	 *            the valid records
	 */
	public void setValidRecords(final Bitmap validRecords) {
		this.validRecords = validRecords;
	}

	@Override
	public Class<?>[] getTypes() {
		final Class<?>[] res;

		if (query.isTransposed()) {
			res = new Class<?>[4];
			res[0] = String.class;
			res[1] = Double.class;
			res[2] = String.class;
			res[3] = timeSeriesCollection.getLabelValueType();

			return res;
		} else {
			final int sizeOfLabels = timeSeriesCollection.sizeOfLabels();
			res = new Class<?>[sizeOfLabels + 1];

			// fill the array with doubles
			Arrays.fill(res, Double.class);

			// the first one will be the identifier so reset
			res[0] = String.class;

			return res;
		}
	}

	@Override
	public String[] getNames() {
		if (query.isTransposed()) {
			return new String[] { "ID", "VALUE", "LABEL", "RAWLABEL" };
		} else {

			// get the labels and add the identifier label to it
			final String[] labels = timeSeriesCollection.getLabels();
			final String[] res = new String[labels.length + 1];
			System.arraycopy(labels, 0, res, 1, labels.length);
			res[0] = "ID";

			return res;
		}
	}

	@Override
	public int[] getCollectedIds() {
		return null;
	}

	@Override
	public Iterator<Object[]> iterator() {
		if (query.isTransposed()) {

			return new Iterator<Object[]>() {
				private final int maxPosition = timeSeriesCollection
						.sizeOfLabels();
				private final Iterator<TimeSeries> it = timeSeriesCollection
						.iterator();

				private boolean eoc = false;
				private int currentPos = -1;
				private TimeSeries currentTimeSeries = null;

				@Override
				public boolean hasNext() {
					if (currentTimeSeries == null) {
						moveToNextPos();
					}

					if (!eoc) {
						eoc = !(currentPos < maxPosition) && !it.hasNext();
					}
					return !eoc;
				}

				@Override
				public Object[] next() {
					if (!hasNext()) {
						throw new IllegalStateException(
								"No next value available.");
					}

					// create the Object
					final Object[] res = new Object[4];
					res[0] = currentTimeSeries.getId();
					res[1] = currentTimeSeries.getValue(currentPos);
					res[2] = timeSeriesCollection.getLabel(currentPos);
					res[3] = timeSeriesCollection.getLabelValue(currentPos);

					// move to the next position
					moveToNextPos();

					return res;
				}

				protected void moveToNextPos() {

					// get a valid timeSeries
					if (currentTimeSeries == null
							|| !(currentPos + 1 < maxPosition)) {
						
						if (it.hasNext()) {
							currentTimeSeries = it.next();
							currentPos = 0;
						} else {
							eoc = true;
						}
					} else {
						currentPos++;
					}
				}

				@Override
				public void remove() {
					throw new UnsupportedOperationException(
							"Remove is not supported.");
				}
			};
		} else {

			return new Iterator<Object[]>() {
				private final Iterator<TimeSeries> it = timeSeriesCollection
						.iterator();

				@Override
				public boolean hasNext() {
					return it.hasNext();
				}

				@Override
				public Object[] next() {
					final TimeSeries ts = it.next();

					// convert the series
					final Object[] res = new Object[ts.size() + 1];
					res[0] = ts.getId();
					for (int i = 0; i < ts.size(); i++) {
						res[i + 1] = ts.getValue(i);
					}

					// return the result
					return res;
				}

				@Override
				public void remove() {
					throw new UnsupportedOperationException(
							"Remove is not supported.");
				}
			};
		}
	}
}
