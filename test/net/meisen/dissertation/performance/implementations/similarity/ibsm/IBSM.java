package net.meisen.dissertation.performance.implementations.similarity.ibsm;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import net.meisen.dissertation.impl.measures.Count;
import net.meisen.dissertation.impl.measures.Max;
import net.meisen.dissertation.impl.measures.Min;
import net.meisen.dissertation.impl.measures.Sum;
import net.meisen.dissertation.impl.parser.query.BaseIntervalValue;
import net.meisen.dissertation.impl.parser.query.Interval;
import net.meisen.dissertation.impl.parser.query.select.DescriptorComperator;
import net.meisen.dissertation.impl.parser.query.select.DimensionComperator;
import net.meisen.dissertation.impl.parser.query.select.IComperator;
import net.meisen.dissertation.impl.parser.query.select.SelectQuery;
import net.meisen.dissertation.impl.parser.query.select.logical.DescriptorLogicTree;
import net.meisen.dissertation.impl.parser.query.select.logical.ILogicalTreeElement;
import net.meisen.dissertation.impl.parser.query.select.logical.LogicalOperator;
import net.meisen.dissertation.impl.parser.query.select.logical.LogicalOperatorNode;
import net.meisen.dissertation.impl.parser.query.select.measures.DescriptorLeaf;
import net.meisen.dissertation.impl.parser.query.select.measures.DescriptorMathTree;
import net.meisen.dissertation.impl.parser.query.select.measures.IMathTreeElement;
import net.meisen.dissertation.impl.parser.query.select.measures.MathOperator;
import net.meisen.dissertation.impl.parser.query.select.measures.MathOperatorNode;
import net.meisen.dissertation.model.data.IntervalModel;
import net.meisen.dissertation.model.data.TidaModel;
import net.meisen.dissertation.model.measures.IAggregationFunction;
import net.meisen.dissertation.model.time.mapper.BaseMapper;
import net.meisen.dissertation.performance.implementations.helper.IntervalTree;
import net.meisen.dissertation.performance.implementations.helper.IntervalTree.IntervalData;
import net.meisen.dissertation.performance.implementations.similarity.EventTable;
import net.meisen.dissertation.performance.implementations.similarity.IValueCalculator;
import net.meisen.general.genmisc.types.Numbers;

/**
 * An implementation of the {@code Interval-Based Sequence Matching} algorithm.
 * The implementation supports dynamic creation of e-sequences, i.e. the
 * database does not contain e-sequences directly, instead the sequences are
 * defined based on a {@code SelectQuery}.<br/>
 * <br/>
 * The implementation is based (without reductions) from Kotsifakos et al.,
 * 2013, <i>IBSM: Interval-Based Sequence Matching</i>.
 * 
 * @author pmeisen
 * 
 */
public class IBSM {
	@SuppressWarnings("javadoc")
	protected final IntervalTree<Integer> iTree;

	@SuppressWarnings("javadoc")
	protected final TidaModel model;
	@SuppressWarnings("javadoc")
	protected final List<Map<String, Object>> records;

	@SuppressWarnings("javadoc")
	protected final String start;
	@SuppressWarnings("javadoc")
	protected final String end;
	@SuppressWarnings("javadoc")
	protected final List<String> labels;

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
		final IValueCalculator caculator = getCalculator(query);
		while (it.hasNext()) {
			final long[] wnd = it.next();

			// make sure all the eSequences are of the same size
			if (!def.isValidSize(wnd)) {
				continue;
			}

			eventTables.add(createEventTable(query, wnd, caculator));
		}

		return eventTables;
	}

	/**
	 * Gets the calculator to be used.
	 * 
	 * @param query
	 *            the query to create the calculator for
	 * 
	 * @return the created calculator
	 */
	protected IValueCalculator getCalculator(final SelectQuery query) {

		// get the measure defined
		final List<DescriptorMathTree> measures = query.getMeasures();
		assert measures.size() == 1;
		final DescriptorMathTree measure = measures.get(0);
		final IMathTreeElement node = measure.getFirst();
		assert node instanceof MathOperatorNode;
		final MathOperator op = ((MathOperatorNode) node).get();
		assert op.isFunction();
		final IAggregationFunction func = op.getFunction();
		final List<IMathTreeElement> children = ((MathOperatorNode) node)
				.getChildren();
		assert children.size() == 1;
		assert children.get(0) instanceof DescriptorLeaf;
		final String desc = children.get(0).toString();

		if (children.get(0) instanceof DescriptorLeaf == false) {
			throw new UnsupportedOperationException(
					"Currently not implemented: " + measure + ".");
		}

		return new IValueCalculator() {

			@Override
			public double getDefaultValue() {
				if (func instanceof Min || func instanceof Max) {
					return Double.NaN;
				} else {
					return 0.0;
				}
			}

			@Override
			public double calcValue(final double curValue,
					final Map<String, Object> record) {
				if (func instanceof Count) {
					return curValue + 1.0;
				} else if (func instanceof Sum) {
					final Object value = record.get(desc);

					if (value instanceof Double) {
						return curValue + (Double) record.get(desc);
					} else {
						return curValue + 1.0;
					}
				} else if (func instanceof Max) {
					final Object value = record.get(desc);

					if (value instanceof Double) {
						if (Double.isNaN(curValue)) {
							return (Double) value;
						} else {
							return Math
									.max(curValue, (Double) record.get(desc));
						}
					} else {
						return 1.0;
					}
				} else if (func instanceof Min) {
					final Object value = record.get(desc);

					if (value instanceof Double) {
						if (Double.isNaN(curValue)) {
							return (Double) value;
						} else {
							return Math
									.min(curValue, (Double) record.get(desc));
						}
					} else {
						return 1.0;
					}
				} else {
					throw new UnsupportedOperationException(
							"Currently not implemented: " + func + ".");
				}
			}

			@Override
			public double getInitValue() {
				if (func instanceof Min || func instanceof Max) {
					return Double.NaN;
				} else {
					return 0.0;
				}
			}
		};
	}

	/**
	 * Creates the event-table for the specified bound.
	 * 
	 * @param query
	 *            the query
	 * @param wnd
	 *            the bound (i.e. window)
	 * @param calculator
	 *            the calculator to be used
	 * 
	 * @return the create table
	 */
	protected EventTable createEventTable(final SelectQuery query,
			final long[] wnd, final IValueCalculator calculator) {
		final BaseMapper<?> mapper = model.getIntervalModel()
				.getTimelineMapper();

		// create the interval
		final BaseIntervalValue<?> start = BaseIntervalValue.createVal(mapper
				.resolve(wnd[0]));
		final BaseIntervalValue<?> end = BaseIntervalValue.createVal(mapper
				.resolve(wnd[1]));
		@SuppressWarnings({ "rawtypes", "unchecked" })
		final Interval<?> interval = new Interval(start, end);

		// create an empty eventTable
		final EventTable eventTable = new EventTable(interval.toString(),
				wnd[1] - wnd[0] + 1, calculator);

		// get the records
		final List<Map<String, Object>> res = this.iTree.query(wnd[0], wnd[1],
				records);
		if (res != null) {
			for (final Map<String, Object> record : res) {
				addEventFromRecord(query, record, wnd, eventTable);
			}
		}

		return eventTable;
	}

	/**
	 * Adds the event specified by the {@code record} for the specified
	 * {@code window} into the specified {@code EventTable}.
	 * 
	 * @param query
	 *            the query
	 * @param record
	 *            the record to be added
	 * @param wnd
	 *            the window
	 * @param eventTable
	 *            the table to add to
	 */
	protected void addEventFromRecord(final SelectQuery query,
			final Map<String, Object> record, final long[] wnd,
			final EventTable eventTable) {
		final IntervalModel intervalModel = model.getIntervalModel();
		final BaseMapper<?> mapper = intervalModel.getTimelineMapper();

		// check the filter
		if (!checkFilter(query.getFilter(), record, null)) {
			return;
		}

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
		eventTable.addEvent(record, Numbers.castToInt(start - wnd[0]),
				Numbers.castToInt(end - wnd[0]), vals);
	}

	/**
	 * Checks if the specified filter is valid.
	 * 
	 * @param filter
	 *            the filter to be checked
	 * @param record
	 *            the record
	 * @param node
	 *            the current node, can be {@code null} if started from root
	 * 
	 * @return {@code true} if the filter is valid, otherwise {@code false}
	 */
	protected boolean checkFilter(final DescriptorLogicTree filter,
			final Map<String, Object> record, LogicalOperatorNode node) {
		if (node == null) {
			node = filter.getRoot();

			// check if we even have a filter
			if (node.getChildren().size() == 0) {
				return true;
			}
		}

		final List<Boolean> values = new ArrayList<Boolean>();
		final List<ILogicalTreeElement> children = node.getChildren();
		for (int i = children.size(); i > 0; i--) {
			final ILogicalTreeElement child = children.get(i - 1);

			if (child instanceof LogicalOperatorNode) {
				values.add(checkFilter(filter, record,
						(LogicalOperatorNode) child));
			} else if (child instanceof net.meisen.dissertation.impl.parser.query.select.logical.DescriptorLeaf) {
				final net.meisen.dissertation.impl.parser.query.select.logical.DescriptorLeaf leaf = (net.meisen.dissertation.impl.parser.query.select.logical.DescriptorLeaf) child;
				final IComperator cmp = leaf.get();

				if (cmp instanceof DescriptorComperator) {
					final DescriptorComperator dCmp = (DescriptorComperator) cmp;

					// get the value of the record
					Object value = record.get(dCmp.getId());

					// check the value
					if (value == null) {
						values.add(false);
					} else if (dCmp.containsWildchar()) {
						values.add(value.toString().matches(dCmp.getValue()));
					} else {
						values.add(value.toString().equals(dCmp.getValue()));
					}
				} else if (cmp instanceof DimensionComperator) {
					assert false;
				} else {
					assert false;
				}
			} else {
				assert false;
			}
		}

		final LogicalOperator lo = node.get();
		if (LogicalOperator.NOT.equals(lo)) {
			if (values.size() != 1) {
				assert false;
			}

			// apply the not
			return !values.get(0);
		} else {
			final int size = values.size();

			if (size < 1) {
				assert false;
			} else if (size > 1) {
				if (LogicalOperator.AND.equals(lo)) {
					for (final Boolean val : values) {
						if (!val) {
							return false;
						}
					}
					return true;
				} else if (LogicalOperator.OR.equals(lo)) {
					for (final Boolean val : values) {
						if (val) {
							return val;
						}
					}
					return false;
				} else {
					assert false;
				}
			} else {
				return values.get(0);
			}
		}
		return false;
	}
}
