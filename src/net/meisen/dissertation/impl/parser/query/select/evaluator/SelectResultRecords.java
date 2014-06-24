package net.meisen.dissertation.impl.parser.query.select.evaluator;

import java.util.Iterator;

import net.meisen.dissertation.exceptions.QueryEvaluationException;
import net.meisen.dissertation.impl.parser.query.select.ResultType;
import net.meisen.dissertation.impl.parser.query.select.SelectQuery;
import net.meisen.dissertation.impl.parser.query.select.SelectResult;
import net.meisen.dissertation.model.data.TidaModel;
import net.meisen.dissertation.model.indexes.datarecord.slices.Bitmap;
import net.meisen.dissertation.model.indexes.datarecord.slices.IIntIterator;
import net.meisen.general.genmisc.exceptions.ForwardedRuntimeException;

public class SelectResultRecords extends SelectResult {

	private Bitmap recordsBitmap;

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
		// TODO Auto-generated method stub
		return new Class<?>[] { Integer.class };
	}

	@Override
	public String[] getNames() {
		// TODO Auto-generated method stub
		return new String[] { "ID" };
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

				return new Object[] { id };
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

		/*
		 * Just an idea, maybe we just return the Descriptors and the Keys as
		 * well as the Interval. Well the Interval could be a problem... because
		 * we don't have it anywhere yet...
		 */

		// get the types

		// get the names

		// get the records
		this.recordsBitmap = recordsEvaluator.evaluateInterval(getQuery()
				.getInterval(), this);
	}
}
