package net.meisen.dissertation.performance.implementations.similarity.mbsm;

import gnu.trove.map.hash.TIntObjectHashMap;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import com.sun.org.apache.bcel.internal.generic.GETSTATIC;

import net.meisen.dissertation.impl.parser.query.BaseIntervalValue;
import net.meisen.dissertation.impl.parser.query.Interval;
import net.meisen.dissertation.impl.parser.query.select.SelectQuery;
import net.meisen.dissertation.impl.time.series.TimeSeries;
import net.meisen.dissertation.impl.time.series.TimeSeriesCollection;
import net.meisen.dissertation.model.data.IntervalModel;
import net.meisen.dissertation.model.data.TidaModel;
import net.meisen.dissertation.model.time.mapper.BaseMapper;
import net.meisen.dissertation.performance.implementations.IRecordsFilter;
import net.meisen.dissertation.performance.implementations.RecordBasedImplementation;
import net.meisen.dissertation.performance.implementations.helper.IntervalTree;
import net.meisen.dissertation.performance.implementations.helper.IntervalTree.IntervalData;
import net.meisen.dissertation.performance.implementations.similarity.EventTable;
import net.meisen.dissertation.performance.implementations.similarity.ibsm.ESequenceDefinition;
import net.meisen.general.genmisc.types.Numbers;

/**
 * A measure-based matcher of time interval data records, using the idea of
 * {@code EventTable} instances as defined by the {@code IBSM}.
 * 
 * @author pmeisen
 * 
 */
public class MBSM extends RecordBasedImplementation {
	private final IntervalTree<Integer> iTree;
	private final IntervalModel intervalModel;

	/**
	 * Constructor to create an {@code MBSM} matcher.
	 * 
	 * @param model
	 *            the model
	 * @param records
	 *            the records
	 * @param start
	 *            the field of the start
	 * @param end
	 *            the field of the end
	 */
	public MBSM(final TidaModel model, final List<Map<String, Object>> records,
			final String start, final String end) {
		super(records, start, end, 0, 1, null, model.getDimensionModel(), model
				.getIndexFactory(), model.getIntervalModel()
				.getTimelineMapper());

		this.iTree = new IntervalTree<Integer>();
		this.intervalModel = model.getIntervalModel();
	}

	/**
	 * Fills the {@code IntervalTree} with the specified records.
	 */
	public void fillIntervalTree() {
		this.iTree.fillTree(transform(data));
	}

	protected List<IntervalData<Integer>> transform(
			final List<Map<String, Object>> records) {
		int i = 0;

		final List<IntervalData<Integer>> list = new ArrayList<IntervalData<Integer>>();
		for (final Map<String, Object> record : records) {
			final Object rStart = record.get(this.start);
			final Object rEnd = record.get(this.end);

			final long start = mapper.mapToLong(rStart);
			final long end = mapper.mapToLong(rEnd);

			list.add(new IntervalData<Integer>(start, end, i));
			i++;
		}

		return list;
	}

	/**
	 * Create the different event-tables available.
	 * 
	 * @param query
	 *            the query to create the tables for
	 * 
	 * @return the created tables
	 */
	public List<EventTable> createEventTables(final SelectQuery query) {
		final List<EventTable> eventTables = new ArrayList<EventTable>();
		final Interval<?> queryInterval = query.getInterval();

		// create a query for each part of the definition
		final ESequenceDefinition def = new ESequenceDefinition(intervalModel,
				queryInterval);

		// iterate over the records and fill the table
		final Iterator<long[]> it = def.iterator();
		while (it.hasNext()) {
			final long[] wnd = it.next();

			// make sure all the eSequences are of the same size
			if (!def.isValidSize(wnd)) {
				continue;
			}

			// create the new query
			final SelectQuery wndQuery = new SelectQuery();
			wndQuery.setResultType(query.getResultType());
			wndQuery.setModelId(query.getModelId());
			wndQuery.setGroup(wndQuery.getGroup());
			wndQuery.setFilter(query.getFilter());
			wndQuery.setMeasures(query.getMeasures());
			wndQuery.setMeasureDimension(query.getMeasureDimension());

			// create the interval
			final BaseIntervalValue<?> start = BaseIntervalValue
					.createVal(mapper.resolve(wnd[0]));
			final BaseIntervalValue<?> end = BaseIntervalValue.createVal(mapper
					.resolve(wnd[1]));
			@SuppressWarnings({ "rawtypes", "unchecked" })
			Interval<?> interval = new Interval(start, end);
			wndQuery.setInterval(interval);

			final TimeSeriesCollection tsc = run(wndQuery);
			if (def.getWindowSize() != tsc.sizeOfLabels()) {
				throw new IllegalStateException(
						"TimeSeries size is unequal to window-size ("
								+ tsc.sizeOfLabels() + " vs. "
								+ def.getWindowSize() + ").");
			}
			eventTables.addAll(createEventTables(interval, tsc));
		}

		return eventTables;
	}

	/**
	 * Create the {@code EventTable} for the specified
	 * {@code TimeSeriesCollection}.
	 * 
	 * @param interval
	 *            the interval the tsc was created for
	 * @param tsc
	 *            the collection to create the table for
	 * @return the created {@code EventTable}
	 */
	protected List<EventTable> createEventTables(final Interval<?> interval,
			final TimeSeriesCollection tsc) {
		final List<EventTable> eventTables = new ArrayList<EventTable>();
		final String name = interval.toString() + " ";

		for (final TimeSeries ts : tsc) {
			final EventTable eventTable = new EventTable(name + ts.getId(),
					ts.size());

			// create the label
			final List<Object> values = new ArrayList<Object>();
			values.add(ts.getId());

			// add the calculated values
			for (int i = 0; i < ts.size(); i++) {
				eventTable.setValue(values, i, ts.getValue(i));
			}

			eventTables.add(eventTable);
		}

		return eventTables;
	}

	@Override
	public TimeSeriesCollection run(final SelectQuery query) {
		return measure(query);
	}

	@Override
	protected TimeSeriesCollection measure(final SelectQuery query) {
		return this.calculateMeasures(query, new IRecordsFilter() {

			@SuppressWarnings("rawtypes")
			private List[] container = null;
			private int posStart = -1;

			@Override
			public List<Map<String, Object>> apply(final long start,
					final long end) {
				final List<Map<String, Object>> res = iTree.query(start, end,
						data);
				if (container == null) {
					container = new List[(int) (end - start + 1)];
					posStart = (int) start;

					for (final Map<String, Object> record : res) {

						// get the needed values from the record
						long s = mapper.mapToLong(record.get(getStartField()));
						long e = mapper.mapToLong(record.get(getEndField()));

						// make sure we don't move out of the window
						s = Math.max(s, start);
						e = Math.min(e, end);

						// add the record and normalize the start and end
						for (int i = (int) s; i <= e; i++) {
							final int pos = (int) (i - posStart);
							@SuppressWarnings("unchecked")
							List<Map<String, Object>> values = container[pos];
							if (values == null) {
								values = new ArrayList<Map<String, Object>>();
								container[pos] = values;
							}

							values.add(record);
						}
					}
				}

				return res;
			}

			@SuppressWarnings("unchecked")
			@Override
			public List<Map<String, Object>> apply(final long start,
					final long end, final List<Map<String, Object>> records) {
				assert start == end;

				final List<Map<String, Object>> res = container[(int) (start - posStart)];
				if (res == null) {
					return Collections.<Map<String, Object>> emptyList();
				} else {
					return res;
				}
			}

			@Override
			public boolean incSupport() {
				return true;
			}
		});
	}
}
