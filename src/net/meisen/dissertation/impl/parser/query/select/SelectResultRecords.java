package net.meisen.dissertation.impl.parser.query.select;

import java.util.Iterator;

import net.meisen.dissertation.exceptions.QueryEvaluationException;
import net.meisen.dissertation.impl.parser.query.select.evaluator.RecordsEvaluator;
import net.meisen.dissertation.model.data.FieldNameGenerator;
import net.meisen.dissertation.model.data.TidaModel;
import net.meisen.dissertation.model.indexes.datarecord.TidaIndex;
import net.meisen.dissertation.model.indexes.datarecord.slices.Bitmap;
import net.meisen.dissertation.model.util.IIntIterator;
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

	private Class<?>[] types;
	private String[] names;

	/**
	 * Constructor specifying the {@code query}, which {@code this} is the
	 * result from.
	 * 
	 * @param query
	 *            the {@code SelectQuery} defining the query of {@code this}
	 */
	public SelectResultRecords(final SelectQuery query) {
		super(query);

		if (!SelectResultType.RECORDS.equals(query.getResultType())) {
			throw new ForwardedRuntimeException(QueryEvaluationException.class,
					1019, SelectResultType.RECORDS.toString(), query.getResultType()
							.toString());
		}
	}

	@Override
	public Class<?>[] getTypes() {
		if (types == null) {
			if (getQuery().isCount()) {
				types = new Class<?>[] { Integer.class };
			} else if (getQuery().isIdsOnly()) {
				types = new Class<?>[] { Integer.class };
			} else {
				types = idx.getRecordTypes();
			}
		}

		return types;
	}

	@Override
	public String[] getNames() {
		if (names == null) {
			if (getQuery().isCount()) {
				names = new String[] { FieldNameGenerator.get()
						.getCountFieldName() };
			} else if (getQuery().isIdsOnly()) {
				names = new String[] { FieldNameGenerator.get()
						.getIdFieldName() };
			} else {
				names = idx.getRecordNames();
			}
		}

		return names;
	}

	@Override
	public Iterator<Object[]> iterator() {

		if (getQuery().isCount()) {

			return new Iterator<Object[]>() {
				private final int count = recordsBitmap.determineCardinality();

				private boolean next = true;

				@Override
				public boolean hasNext() {
					if (next) {
						next = false;
						return true;
					} else {
						return false;
					}
				}

				@Override
				public Object[] next() {
					next = false;
					return new Object[] { count };
				}

				@Override
				public void remove() {
					throw new UnsupportedOperationException(
							"Remove is not supported.");
				}
			};
		} else if (getQuery().isIdsOnly()) {

			return new Iterator<Object[]>() {
				private final IIntIterator it = recordsBitmap.intIterator();

				@Override
				public boolean hasNext() {
					return it.hasNext();
				}

				@Override
				public Object[] next() {
					return new Object[] { it.next() };
				}

				@Override
				public void remove() {
					throw new UnsupportedOperationException(
							"Remove is not supported.");
				}
			};
		} else {

			return new Iterator<Object[]>() {
				private final IIntIterator it = recordsBitmap.intIterator();

				@Override
				public boolean hasNext() {
					return it.hasNext();
				}

				@Override
				public Object[] next() {
					return idx.getRecordAsArray(it.next());
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
		final RecordsEvaluator recordsEvaluator = new RecordsEvaluator(model);

		// get the records
		this.recordsBitmap = recordsEvaluator.evaluateInterval(getQuery()
				.getInterval(), getQuery().getIntervalRelation(), this);

		// get the types and names
		this.idx = model.getIndex();
	}

	/**
	 * Gets the bitmap with the selected records.
	 * 
	 * @return the bitmap with the selected records
	 */
	public Bitmap getSelectedRecords() {
		return recordsBitmap;
	}
}
