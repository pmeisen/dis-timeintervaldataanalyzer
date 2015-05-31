package net.meisen.dissertation.performance.implementations;

import static org.junit.Assert.fail;

import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import java.util.Map;
import java.util.Set;

import net.meisen.dissertation.impl.measures.Count;
import net.meisen.dissertation.impl.measures.Max;
import net.meisen.dissertation.impl.measures.Min;
import net.meisen.dissertation.impl.measures.Sum;
import net.meisen.dissertation.impl.parser.query.DimensionSelector;
import net.meisen.dissertation.impl.parser.query.select.DescriptorComperator;
import net.meisen.dissertation.impl.parser.query.select.DimensionComperator;
import net.meisen.dissertation.impl.parser.query.select.IComperator;
import net.meisen.dissertation.impl.parser.query.select.SelectQuery;
import net.meisen.dissertation.impl.parser.query.select.logical.DescriptorLeaf;
import net.meisen.dissertation.impl.parser.query.select.logical.DescriptorLogicTree;
import net.meisen.dissertation.impl.parser.query.select.logical.ILogicalTreeElement;
import net.meisen.dissertation.impl.parser.query.select.logical.LogicalOperator;
import net.meisen.dissertation.impl.parser.query.select.logical.LogicalOperatorNode;
import net.meisen.dissertation.impl.parser.query.select.measures.ArithmeticOperator;
import net.meisen.dissertation.impl.parser.query.select.measures.DescriptorMathTree;
import net.meisen.dissertation.impl.parser.query.select.measures.IMathTreeElement;
import net.meisen.dissertation.impl.parser.query.select.measures.MathOperator;
import net.meisen.dissertation.impl.parser.query.select.measures.MathOperatorNode;
import net.meisen.dissertation.impl.time.series.TimeSeries;
import net.meisen.dissertation.impl.time.series.TimeSeriesCollection;
import net.meisen.dissertation.model.data.DimensionModel;
import net.meisen.dissertation.model.data.TidaModel;
import net.meisen.dissertation.model.dimensions.TimeLevelMember;
import net.meisen.dissertation.model.dimensions.TimeMemberRange;
import net.meisen.dissertation.model.indexes.BaseIndexFactory;
import net.meisen.dissertation.model.measures.IAggregationFunction;
import net.meisen.dissertation.model.parser.query.IQueryFactory;
import net.meisen.dissertation.model.time.mapper.BaseMapper;
import net.meisen.general.genmisc.types.Numbers;

@SuppressWarnings("javadoc")
public abstract class RecordBasedImplementation extends
		BaseImplementation<List<Map<String, Object>>> {
	protected final String start;
	protected final String end;

	public RecordBasedImplementation(final TidaModel model,
			final List<Map<String, Object>> records, final String start,
			final String end, final int initRuns, final int runs,
			final IQueryFactory queryFactory) {
		this(records, start, end, initRuns, runs, queryFactory, model
				.getDimensionModel(), model.getIndexFactory(), model
				.getIntervalModel().getTimelineMapper());
	}

	public RecordBasedImplementation(final List<Map<String, Object>> records,
			final String start, final String end, final int initRuns,
			final int runs, final IQueryFactory queryFactory,
			final DimensionModel dimModel, final BaseIndexFactory factory,
			final BaseMapper<?> mapper) {
		super(records, initRuns, runs, queryFactory, dimModel, factory, mapper);

		this.start = start;
		this.end = end;
	}

	protected boolean checkDate(final BaseMapper<?> m, final Date sDate,
			final Date eDate, final Map<String, Object> record) {

		final Object sIntervalDate = record.get(start);
		final Object eIntervalDate = record.get(end);

		final long sTw = m.mapToLong(sDate);
		final long eTw = m.mapToLong(eDate);
		final long sInterval = m.mapToLong(sIntervalDate);
		final long eInterval = m.mapToLong(eIntervalDate);

		return sTw <= eInterval && eTw >= sInterval;
	}

	protected boolean checkFilter(final DescriptorLogicTree filter,
			final Map<String, Object> record, LogicalOperatorNode node) {
		if (node == null) {
			node = filter.getRoot();
			
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
			} else if (child instanceof DescriptorLeaf) {
				final DescriptorLeaf leaf = (DescriptorLeaf) child;
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
					final DimensionComperator dCmp = (DimensionComperator) cmp;
					final DimensionSelector dSel = dCmp.getDimension();

					if (dSel.toString().equals("WA.LOC.TYPE")
							&& dCmp.getRawValue().equals("Gate")) {
						final Object value = record.get("WORKAREA");
						values.add(value.toString().matches("BIE\\..*"));
					} else if (dSel.toString().equals("WA.LOC.TYPE")
							&& dCmp.getRawValue().equals("Ramp")) {
						final Object value = record.get("WORKAREA");
						values.add(value.toString().matches("EDI\\..*"));
					} else {
						fail("Not avialable currently '" + dSel + "', '"
								+ dCmp.getRawValue() + "'");
					}
				} else {
					fail("Invalid compeartor");
				}
			} else {
				fail("Invalid node '" + child + "'");
			}
		}

		final LogicalOperator lo = node.get();
		if (LogicalOperator.NOT.equals(lo)) {
			if (values.size() != 1) {
				fail("Invalid size for not '" + values + "'");
			}

			// apply the not
			return !values.get(0);
		} else {
			final int size = values.size();

			if (size < 1) {
				fail("Invalid formular");
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
					fail("Invalid operator '" + lo + "'");
				}
			} else {
				return values.get(0);
			}
		}

		return false;
	}

	protected TimeSeriesCollection calculateMeasures(final SelectQuery query,
			final IRecordsFilter filter) {
		final long[] bounds = mapper.getBounds(query.getInterval());
		final long s = bounds[0];
		final long e = bounds[1];

		final TimeSeriesCollection tsc;
		if (query.getMeasureDimension() == null) {
			tsc = new TimeSeriesCollection(Numbers.castToInt(e - s + 1), query
					.getInterval().getStart().getClass(), factory);

			final List<Map<String, Object>> allRecords;
			if (filter.incSupport()) {
				allRecords = filter.apply(s, e);
			} else {
				allRecords = null;
			}
			for (long i = s; i <= e; i++) {
				final List<Map<String, Object>> tpRecords;
				if (filter.incSupport()) {
					tpRecords = filter.apply(i, i, allRecords);
				} else {
					tpRecords = filter.apply(i, i);
				}

				// set the label
				final Object labelValue = mapper.resolve(i);
				tsc.setLabel(Numbers.castToInt(i - s),
						mapper.format(labelValue), labelValue);

				for (final DescriptorMathTree measure : query.getMeasures()) {
					final IMathTreeElement node = ((MathOperatorNode) measure
							.getRoot()).getChild(0);
					final List<Double> res = calculateMeasure(tpRecords, node);
					assert 1 == res.size();

					// set the value within the series
					final TimeSeries ts = getTimeSeries(tsc, measure.getId());
					ts.setValue(Numbers.castToInt(i - s), res.get(0)
							.doubleValue());
				}
			}
		} else {
			final Set<TimeLevelMember> members = dimModel.getTimeMembers(
					query.getMeasureDimension(), s, e);
			tsc = new TimeSeriesCollection(members.size(), String.class,
					factory);

			int i = 0;
			for (final TimeLevelMember member : members) {
				assert 1 == member.getRanges().size();
				final TimeMemberRange range = member.getRange(0);

				// set the label
				tsc.setLabel(i, member.getName(), member.getId());

				for (final DescriptorMathTree measure : query.getMeasures()) {
					final IMathTreeElement node = ((MathOperatorNode) measure
							.getRoot()).getChild(0);

					if (measure.isSimple()) {
						final List<Map<String, Object>> tpRecords = filter
								.apply(range.getStart(), range.getEnd());
						final List<Double> res = calculateMeasure(tpRecords,
								node);
						assert 1 == res.size();

						// set the value within the series
						final TimeSeries ts = getTimeSeries(tsc,
								measure.getId());
						ts.setValue(i, res.get(0).doubleValue());
					} else {
						final IMathTreeElement iNode = ((MathOperatorNode) node)
								.getChild(0);
						final List<Double> values = new ArrayList<Double>();

						final List<Map<String, Object>> tpRecords;
						if (filter.incSupport()) {
							tpRecords = filter.apply(range.getStart(),
									range.getEnd());
						} else {
							tpRecords = null;
						}

						// get the inner values
						for (long r = range.getStart(); r <= range.getEnd(); r++) {
							final List<Map<String, Object>> rangeRecords;
							rangeRecords = filter.apply(r, r, tpRecords);

							final List<Double> res = calculateMeasure(
									rangeRecords, iNode);
							assert 1 == res.size();
							values.add(res.get(0));
						}

						final TimeSeries ts = getTimeSeries(tsc,
								measure.getId());
						if (values.size() < 1) {
							ts.setValue(i, Double.NaN);
						} else {
							final MathOperator mo = ((MathOperatorNode) node)
									.get();
							assert mo.isFunction();

							final IAggregationFunction func = mo.getFunction();
							ts.setValue(i, applyFunction(func, values));
						}
					}
				}

				i++;
			}
		}

		return tsc;
	}

	protected List<Double> calculateMeasure(
			final List<Map<String, Object>> records, final IMathTreeElement node) {

		final List<Double> result = new ArrayList<Double>();
		if (node instanceof net.meisen.dissertation.impl.parser.query.select.measures.DescriptorLeaf) {
			final net.meisen.dissertation.impl.parser.query.select.measures.DescriptorLeaf dLeaf = (net.meisen.dissertation.impl.parser.query.select.measures.DescriptorLeaf) node;

			for (final Map<String, Object> record : records) {

				final Object value = record.get(dLeaf.get());

				if (value instanceof Number) {
					result.add(((Number) value).doubleValue());
				} else if (value == null) {
					result.add(Double.NaN);
				} else {
					result.add(1.0);
				}
			}
		} else if (node instanceof MathOperatorNode) {
			final MathOperatorNode math = (MathOperatorNode) node;

			final List<List<Double>> childrenRes = new ArrayList<List<Double>>();
			for (final IMathTreeElement child : math.getChildren()) {
				childrenRes.add(calculateMeasure(records, child));
			}

			// handle known values
			final MathOperator mo = ((MathOperatorNode) node).get();
			if (mo.isFunction()) {
				if (childrenRes.size() != 1) {
					fail("Invalid definition '" + node + "'");
				}
				final List<Double> childRes = childrenRes.get(0);
				final IAggregationFunction func = mo.getFunction();
				result.add(applyFunction(func, childRes));
			} else {
				final ArithmeticOperator op = mo.getOperator();
				final List<Double> firstRes = childrenRes.get(0);
				final int valueSize = firstRes.size();

				for (int i = 0; i < valueSize; i++) {
					double curResult = firstRes.get(i);
					for (int k = 1; k < childrenRes.size(); k++) {
						curResult = op.apply(curResult,
								childrenRes.get(k).get(i));
					}

					result.add(curResult);
				}
			}
		} else {
			fail("Invalid node '" + node + "' (" + node.getClass().getName()
					+ ")");
		}

		return result;
	}

	protected double applyFunction(final IAggregationFunction func,
			final List<Double> values) {
		double tmpRes = Double.NaN;
		if (func instanceof Sum) {
			tmpRes = 0;
			for (final double res : values) {
				if (Double.isNaN(res)) {
					tmpRes = 0;
					break;
				} else {
					tmpRes = Double.isNaN(tmpRes) ? res : tmpRes + res;
				}
			}
		} else if (func instanceof Count) {
			tmpRes = values.size();
		} else if (func instanceof Min) {
			for (final double res : values) {
				if (Double.isNaN(res)) {
					continue;
				} else {
					tmpRes = Double.isNaN(tmpRes) ? res : (res < tmpRes ? res
							: tmpRes);
				}
			}
		} else if (func instanceof Max) {
			for (final double res : values) {
				if (Double.isNaN(res)) {
					continue;
				} else {
					tmpRes = Double.isNaN(tmpRes) ? res : (res > tmpRes ? res
							: tmpRes);
				}
			}
		} else {
			fail("Unsupported aggregation '" + func.getClass().getSimpleName()
					+ "'");
		}

		return tmpRes;
	}

	public String getStartField() {
		return start;
	}

	public String getEndField() {
		return end;
	}
}
