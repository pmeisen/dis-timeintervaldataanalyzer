package net.meisen.dissertation.impl.parser.query.select.evaluator;

import java.util.Collections;
import java.util.List;

import net.meisen.dissertation.exceptions.QueryEvaluationException;
import net.meisen.dissertation.impl.parser.query.select.SelectResultType;
import net.meisen.dissertation.impl.parser.query.select.SelectQuery;
import net.meisen.dissertation.impl.parser.query.select.SelectResult;
import net.meisen.dissertation.impl.parser.query.select.SelectResultRecords;
import net.meisen.dissertation.impl.parser.query.select.SelectResultTimeSeries;
import net.meisen.dissertation.model.data.TidaModel;
import net.meisen.dissertation.model.indexes.datarecord.slices.Bitmap;
import net.meisen.general.genmisc.exceptions.ForwardedRuntimeException;

/**
 * An evaluator used to evaluate select queries.
 * 
 * @author pmeisen
 * 
 */
public class SelectEvaluator {
	private final TidaModel model;

	/**
	 * Standard constructor, initializes the evaluator to be used with the
	 * specified {@code model}.
	 * 
	 * @param model
	 *            the model to be used with the evaluator
	 */
	public SelectEvaluator(final TidaModel model) {
		this.model = model;
	}

	/**
	 * Prepares an instance of the result to be evaluated. The preparation sets
	 * all the values available by {@code this}, but does not trigger the
	 * {@link SelectResult#determineResult(TidaModel)}.
	 * 
	 * @param query
	 *            the query to prepare the result for
	 * 
	 * @return the prepared result
	 * 
	 * @see #evaluate(SelectQuery)
	 */
	public SelectResult prepareResult(final SelectQuery query) {

		// the result holder
		final SelectResult queryResult = createSelectResult(query);

		// set the valid records
		final Bitmap validRecords = model.getValidRecords();
		queryResult.setValidRecords(validRecords);

		// determine the filter results
		final IBitmapResult filterResult;
		if (query.getRecordIdFilter() != null) {
			filterResult = Bitmap.createBitmap(model.getIndexFactory(),
					query.getRecordIdFilter());
		} else {
			final DescriptorLogicEvaluator descriptorEvaluator = new DescriptorLogicEvaluator(
					model);
			filterResult = descriptorEvaluator.evaluateTree(query.getFilter());
		}
		queryResult.setFilterResult(filterResult);

		// determine the different groups
		final GroupEvaluator groupEvaluator = new GroupEvaluator(model);
		final GroupResult groupResult = groupEvaluator
				.evaluateGroupExpression(query.getGroup());
		queryResult.setGroupResult(groupResult);

		// combine the filter with the valid records
		if (filterResult != null && groupResult != null) {
			final Bitmap filteredValidRecords = Bitmap.combineBitmaps(
					validRecords, filterResult);

			// combine the group and the filteredValid
			final GroupResult filteredGroupResult = new GroupResult();
			for (final GroupResultEntry groupResultEntry : groupResult) {
				final List<String> group = groupResultEntry == null ? Collections
						.<String> emptyList() : groupResultEntry
						.getGroupAsList();

				final Bitmap bmp = Bitmap.combineBitmaps(filteredValidRecords,
						groupResultEntry);
				filteredGroupResult.add(group, bmp);
			}

			queryResult.setFilteredGroupResult(filteredGroupResult);
		} else if (groupResult != null) {
			queryResult.setFilteredGroupResult(groupResult);
		} else if (filterResult != null) {
			final Bitmap filteredValidRecords = Bitmap.combineBitmaps(
					validRecords, filterResult);

			// create a single group
			final GroupResult res = new GroupResult();
			res.add(Collections.<String> emptyList(), filteredValidRecords);

			// set the result as the group-filtered result
			queryResult.setFilteredGroupResult(res);
		} else {

			// create a single group
			final GroupResult res = new GroupResult();
			res.add(Collections.<String> emptyList(), validRecords);

			// set the result as the group-filtered result
			queryResult.setFilteredGroupResult(res);
		}

		return queryResult;
	}

	/**
	 * Evaluates the specified {@code SelectQuery}. That means that the method
	 * first prepares a result (see {@link #prepareResult(SelectQuery)}) and
	 * than starts the determination of the result.
	 * 
	 * @param query
	 *            the query to be evaluated
	 * 
	 * @return the result of the query
	 */
	public SelectResult evaluate(final SelectQuery query) {

		// prepare a result for the query
		final SelectResult queryResult = prepareResult(query);

		// determine the result
		queryResult.determineResult(model);

		return queryResult;
	}

	/**
	 * Factory method used to create the right {@code SelectQuery} instance
	 * depending on the {@code ResultType} of the query
	 * 
	 * @param query
	 *            the {@code SelectQuery} the result will be based one
	 * 
	 * @return the created {@code SelectResult}
	 * 
	 * @see SelectQuery#getResultType()
	 */
	protected SelectResult createSelectResult(final SelectQuery query) {
		final SelectResultType selectResultType = query.getResultType();

		if (SelectResultType.TIMESERIES.equals(selectResultType)) {
			return new SelectResultTimeSeries(query);
		} else if (SelectResultType.RECORDS.equals(selectResultType)) {
			return new SelectResultRecords(query);
		} else {
			throw new ForwardedRuntimeException(QueryEvaluationException.class,
					1018, selectResultType == null ? null
							: selectResultType.toString());
		}
	}
}
