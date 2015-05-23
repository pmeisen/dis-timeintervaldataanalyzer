package net.meisen.dissertation.impl.parser.query.select;

import net.meisen.dissertation.impl.parser.query.select.evaluator.GroupResult;
import net.meisen.dissertation.impl.parser.query.select.evaluator.IBitmapResult;
import net.meisen.dissertation.model.data.TidaModel;
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
public abstract class SelectResult implements IQueryResultSet,
		Iterable<Object[]> {
	private final SelectQuery query;

	private Bitmap validRecords;
	private IBitmapResult filterResult;
	private GroupResult groupResult;
	private GroupResult filteredGroupResult;

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
	public IBitmapResult getFilterResult() {
		return filterResult;
	}

	/**
	 * Sets the result of the filter specified by the query.
	 * 
	 * @param filterResult
	 *            the result of the filtering
	 */
	public void setFilterResult(final IBitmapResult filterResult) {
		this.filterResult = filterResult;
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
	 * Gets the filtered result of the group defined by the query.
	 * 
	 * @return the filtered result of the group
	 */
	public GroupResult getFilteredGroupResult() {
		return filteredGroupResult;
	}

	/**
	 * Sets the result of the filtered group.
	 * 
	 * @param filteredGroupResult
	 *            the filtered grouping result
	 */
	public void setFilteredGroupResult(final GroupResult filteredGroupResult) {
		this.filteredGroupResult = filteredGroupResult;
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
	public int[] getCollectedIds() {
		return null;
	}

	/**
	 * Sets all the values of {@code this} to the values specified by the passed
	 * {@code result}.
	 * 
	 * @param result
	 *            the result to get the values from
	 */
	public void set(final SelectResult result) {
		setFilteredGroupResult(result.getFilteredGroupResult());
		setFilterResult(result.getFilterResult());
		setGroupResult(result.getGroupResult());
		setValidRecords(result.getValidRecords());
	}

	/**
	 * Method used to determine the final result. The method is called after the
	 * {@code validRecords}, {@code filterResult}, and {@code groupResult} are
	 * set.
	 * 
	 * @param model
	 *            the {@code TidaModel} specified in the query
	 */
	public abstract void determineResult(final TidaModel model);

}
