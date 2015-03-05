package net.meisen.dissertation.impl.parser.query.select;

import java.util.Arrays;
import java.util.Iterator;

import net.meisen.dissertation.exceptions.QueryEvaluationException;
import net.meisen.dissertation.impl.parser.query.select.evaluator.TimeSeriesEvaluator;
import net.meisen.dissertation.impl.time.series.TimeSeries;
import net.meisen.dissertation.impl.time.series.TimeSeriesCollection;
import net.meisen.dissertation.model.data.FieldNameGenerator;
import net.meisen.dissertation.model.data.TidaModel;
import net.meisen.general.genmisc.exceptions.ForwardedRuntimeException;

/**
 * Result of a query used to retrieve {@code TimeSeries}.
 * 
 * @author pmeisen
 * 
 */
public class SelectResultTimeSeries extends SelectResult {
	private TimeSeriesCollection timeSeriesCollection;

	/**
	 * Default constructor specifying the {@code query} the result should be
	 * created for.
	 * 
	 * @param query
	 *            the query the result should be created for
	 */
	public SelectResultTimeSeries(final SelectQuery query) {
		super(query);

		if (!SelectResultType.TIMESERIES.equals(query.getResultType())) {
			throw new ForwardedRuntimeException(QueryEvaluationException.class,
					1019, SelectResultType.TIMESERIES.toString(), query
							.getResultType().toString());
		}
	}

	/**
	 * Gets the determined {@code TimeSeries} specified by the query.
	 * 
	 * @return the determined {@code TimeSeries}
	 */
	public TimeSeriesCollection getTimeSeriesResult() {
		return timeSeriesCollection;
	}

	@Override
	public Class<?>[] getTypes() {
		final Class<?>[] res;

		if (getQuery().isTransposed()) {
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
		if (getQuery().isTransposed()) {
			final FieldNameGenerator fg = FieldNameGenerator.get();

			return new String[] { fg.getIdFieldName(), fg.getValueFieldName(),
					fg.getLabelFieldName(), fg.getRawLabelFieldName() };
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
	public Iterator<Object[]> iterator() {
		if (getQuery().isTransposed()) {

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

	@Override
	public void determineResult(final TidaModel model) {
		final TimeSeriesEvaluator timeSeriesEvaluator = new TimeSeriesEvaluator(
				model);
		this.timeSeriesCollection = timeSeriesEvaluator.evaluateInterval(
				getQuery(), this);
	}

	@Override
	public String toString() {
		return timeSeriesCollection.toString();
	}

	/**
	 * Sets the {@code TimeSeriesCollection} of {@code this}.
	 * 
	 * @param timeSeriesCollection
	 *            the collection to be set
	 */
	public void setTimeSeriesCollection(
			final TimeSeriesCollection timeSeriesCollection) {
		this.timeSeriesCollection = timeSeriesCollection;
	}
}
