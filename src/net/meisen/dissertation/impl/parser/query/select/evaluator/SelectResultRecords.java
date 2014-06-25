package net.meisen.dissertation.impl.parser.query.select.evaluator;

import java.util.Iterator;

import net.meisen.dissertation.exceptions.QueryEvaluationException;
import net.meisen.dissertation.impl.parser.query.select.ResultType;
import net.meisen.dissertation.impl.parser.query.select.SelectQuery;
import net.meisen.dissertation.impl.parser.query.select.SelectResult;
import net.meisen.dissertation.model.data.TidaModel;
import net.meisen.dissertation.model.indexes.datarecord.TidaIndex;
import net.meisen.dissertation.model.indexes.datarecord.slices.Bitmap;
import net.meisen.dissertation.model.indexes.datarecord.slices.IIntIterator;
import net.meisen.general.genmisc.exceptions.ForwardedRuntimeException;

/**
 * A result of a {@code SelectQuery} retrieving records.
 * 
 * @author pmeisen
 * 
 */
public class SelectResultRecords extends SelectResult {

	private Bitmap recordsBitmap;
	private TidaIndex idx;

	/**
	 * Constructor specifying the {@code query}, which {@code this} is the
	 * result from.
	 * 
	 * @param query
	 *            the {@code SelectQuery} defining the query of {@code this}
	 */
	public SelectResultRecords(final SelectQuery query) {
		super(query);

		if (!ResultType.RECORDS.equals(query.getResultType())) {
			throw new ForwardedRuntimeException(QueryEvaluationException.class,
					1019, ResultType.RECORDS.toString(), query.getResultType()
							.toString());
		}
	}

	@Override
	public Class<?>[] getTypes() {
		return idx.getRecordTypes();
	}

	@Override
	public String[] getNames() {
		return idx.getRecordNames();
	}

	@Override
	public Iterator<Object[]> iterator() {

		return new Iterator<Object[]>() {
			private final IIntIterator it = recordsBitmap.intIterator();

			@Override
			public boolean hasNext() {
				return it.hasNext();
			}

			@Override
			public Object[] next() {
				final int id = it.next();
				return idx.getRecord(id);
			}

			@Override
			public void remove() {
				throw new UnsupportedOperationException(
						"Remove is not supported.");
			}
		};
	}

	@Override
	public void determineResult(final TidaModel model) {
		final RecordsEvaluator recordsEvaluator = new RecordsEvaluator(model);

		// get the records
		this.recordsBitmap = recordsEvaluator.evaluateInterval(getQuery()
				.getInterval(), this);

		// get the types and names
		this.idx = model.getIndex();
	}

	public Bitmap getSelectedRecords() {
		return recordsBitmap;
	}
}
