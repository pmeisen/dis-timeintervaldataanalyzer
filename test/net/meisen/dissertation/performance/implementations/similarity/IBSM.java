package net.meisen.dissertation.performance.implementations.similarity;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import net.meisen.dissertation.impl.parser.query.select.SelectQuery;
import net.meisen.dissertation.model.data.IntervalModel;
import net.meisen.dissertation.model.data.TidaModel;
import net.meisen.dissertation.model.time.mapper.BaseMapper;
import net.meisen.dissertation.performance.implementations.helper.IntervalTree;
import net.meisen.dissertation.performance.implementations.helper.IntervalTree.IntervalData;
import net.meisen.general.genmisc.types.Numbers;

/**
 * An implementation of the {@code Interval-Based Sequence Matching} algorithm.
 * The implementation supports dynamic creation of e-sequences, i.e. the
 * database does not contain e-sequences directly, instead the sequences are
 * defined based on a {@code SelectQuery}.
 * 
 * @author pmeisen
 * 
 */
public class IBSM {

	private final IntervalTree<Integer> iTree;

	private final TidaModel model;
	private final List<Map<String, Object>> records;

	private final String start;
	private final String end;
	private final List<String> labels;

	/**
	 * Constructor creating a {@code IBSM} instance for the specified model and
	 * the specified records (database). The {@code start}, {@code end} and
	 * {@code labels} specify the fields of the records containing the values.
	 * 
	 * @param model
	 *            the model
	 * @param records
	 *            the database
	 * @param start
	 *            the field of a record containing the start
	 * @param end
	 *            the field of a record containing the end
	 * @param labels
	 *            the fields containing the labels
	 */
	public IBSM(final TidaModel model, final List<Map<String, Object>> records,
			final String start, final String end, final String... labels) {
		this(model, records, start, end, Arrays.asList(labels));
	}

	/**
	 * Constructor creating a {@code IBSM} instance for the specified model and
	 * the specified records (database). The {@code start}, {@code end} and
	 * {@code labels} specify the fields of the records containing the values.
	 * 
	 * @param model
	 *            the model
	 * @param records
	 *            the database
	 * @param start
	 *            the field of a record containing the start
	 * @param end
	 *            the field of a record containing the end
	 * @param labels
	 *            the fields containing the labels
	 */
	public IBSM(final TidaModel model, final List<Map<String, Object>> records,
			final String start, final String end, final List<String> labels) {
		this.model = model;
		this.records = records;

		this.iTree = new IntervalTree<Integer>();
		this.start = start;
		this.end = end;
		this.labels = labels;
	}

	/**
	 * Fills the {@code IntervalTree} with the specified records.
	 */
	public void fillIntervalTree() {
		int i = 0;

		final BaseMapper<?> mapper = model.getIntervalModel()
				.getTimelineMapper();
		final List<IntervalData<Integer>> list = new ArrayList<IntervalData<Integer>>();
		for (final Map<String, Object> record : records) {
			final Object rStart = record.get(this.start);
			final Object rEnd = record.get(this.end);

			final long start = mapper.mapToLong(rStart);
			final long end = mapper.mapToLong(rEnd);

			list.add(new IntervalData<Integer>(start, end, i));
			i++;
		}

		this.iTree.fillTree(list);
	}

	/**
	 * Creates the e-sequences represented as {@code EventTable} for the
	 * specified queries.
	 * 
	 * @param query
	 *            the query to create the e-sequences for
	 * 
	 * @return the e-sequences represented as {@code EventTable} instances
	 */
	public List<EventTable> createEventTables(final SelectQuery query) {
		final List<EventTable> eventTables = new ArrayList<EventTable>();
		final IntervalModel intervalModel = model.getIntervalModel();
		final BaseMapper<?> mapper = intervalModel.getTimelineMapper();

		/*
		 * TODO: we have to decide how the filter, filter-value, group,
		 * group-value is recognized. Additionally, measures are not really
		 * considered.
		 */

		/*
		 * We don't have e-sequences pre-defined. In our definition the
		 * e-sequence definition is created dynamically based on a query. A
		 * e-sequence definition can be understood as the grouping criteria,
		 * specifying which events (records) belong together.
		 */
		final ESequenceDefinition def = new ESequenceDefinition(intervalModel,
				query.getInterval());

		// iterate over the records and fill the table
		final Iterator<long[]> it = def.iterator();
		while (it.hasNext()) {
			final long[] wnd = it.next();

			// make sure all the eSequences are of the same size
			if (!def.isValidSize(wnd)) {
				continue;
			}

			// create an empty eventTable
			final EventTable eventTable = new EventTable(wnd[1] - wnd[0] + 1);
			eventTables.add(eventTable);

			// get the records
			final IntervalData<Integer> res = this.iTree.query(wnd[0], wnd[1]);
			for (final Integer i : res.getData()) {
				final Map<String, Object> record = records.get(i);

				// get the needed values from the record
				long start = mapper.mapToLong(record.get(this.start));
				long end = mapper.mapToLong(record.get(this.end));
				List<Object> vals = new ArrayList<Object>(this.labels.size());
				for (final String label : this.labels) {
					final Object value = record.get(label);
					vals.add(value);
				}

				// make sure we don't move out of the window
				start = Math.max(start, wnd[0]);
				end = Math.min(end, wnd[1]);

				// add the record and normalize the start and end
				eventTable.addEvent(Numbers.castToInt(start - wnd[0]),
						Numbers.castToInt(end - wnd[0]), vals);
			}
		}

		return eventTables;
	}
}
