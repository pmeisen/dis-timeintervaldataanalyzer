package net.meisen.dissertation.impl.parser.query.select;

import java.util.Iterator;

import net.meisen.dissertation.exceptions.QueryEvaluationException;
import net.meisen.dissertation.impl.parser.query.select.evaluator.GroupResult;
import net.meisen.dissertation.impl.parser.query.select.evaluator.GroupResultEntry;
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
					1019, SelectResultType.RECORDS.toString(), query
							.getResultType().toString());
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
			return createCountIterator();
		} else if (getQuery().isIdsOnly()) {
			return createIdIterator();
		} else {
			return createRecordIterator();
		}
	}

	/**
	 * Applies the defined limit (i.e. forwarding to the specified offset) to
	 * the iterator.
	 * 
	 * @param it
	 *            the iterator to apply the limit to
	 * 
	 * @return the passed iterator
	 */
	protected IIntIterator applyLimit(final IIntIterator it) {
		final int offset = getQuery().getOffset();
		for (int i = 0; i < offset && it.hasNext(); i++) {
			it.next();
		}

		return it;
	}

	/**
	 * Creates an iterator used to iterate over the records retrieved by the
	 * statement.
	 * 
	 * @return an iterator returned when the records are requested
	 */
	protected Iterator<Object[]> createRecordIterator() {

		return new Iterator<Object[]>() {
			private final IIntIterator it = applyLimit(recordsBitmap
					.intIterator());
			private final int limit = getQuery().getLimit();

			private int counter = 0;

			@Override
			public boolean hasNext() {
				return (limit < 0 || counter < limit) && it.hasNext();
			}

			@Override
			public Object[] next() {
				counter++;
				return idx.getRecordAsArray(it.next());
			}

			@Override
			public void remove() {
				throw new UnsupportedOperationException(
						"Remove is not supported.");
			}
		};
	}

	/**
	 * Creates an iterator used to iterate over the identifiers retrieved by the
	 * statement.
	 * 
	 * @return an iterator returned when the identifiers are requested
	 */
	protected Iterator<Object[]> createIdIterator() {
		return new Iterator<Object[]>() {
			private final IIntIterator it = recordsBitmap.intIterator();
			private final int limit = getQuery().getLimit();

			private int counter = 0;

			@Override
			public boolean hasNext() {
				return (limit < 0 || counter < limit) && it.hasNext();
			}

			@Override
			public Object[] next() {
				counter++;
				return new Object[] { it.next() };
			}

			@Override
			public void remove() {
				throw new UnsupportedOperationException(
						"Remove is not supported.");
			}
		};
	}

	/**
	 * Method used to create an iterator providing count informations.
	 * 
	 * @return the iterator used as reply to a count statement
	 */
	protected Iterator<Object[]> createCountIterator() {
		return new Iterator<Object[]>() {
			private final int count = count();

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
	}

	@Override
	public void determineResult(final TidaModel model) {
		final RecordsEvaluator recordsEvaluator = new RecordsEvaluator(model);

		// get the filtered bitmap
		final GroupResult filteredGroupResult = getFilteredGroupResult();
		final GroupResultEntry singleGroupEntry = filteredGroupResult
				.getSingleGroupEntry();
		if (singleGroupEntry == null) {
			throw new ForwardedRuntimeException(QueryEvaluationException.class,
					1028, getQuery(), filteredGroupResult);
		}
		final Bitmap filteredBitmap = singleGroupEntry.getBitmap();

		// get the records
		this.recordsBitmap = recordsEvaluator.evaluateInterval(getQuery(),
				filteredBitmap);

		// get the types and names
		this.idx = model.getIndex();
	}

	/**
	 * Gets the bitmap with the selected records. The returned bitmap does not
	 * have any limit applied.
	 * 
	 * @return the bitmap with the selected records
	 */
	public Bitmap getSelectedRecords() {
		return recordsBitmap;
	}

	/**
	 * The amount of records retrieved, including any defined limit.
	 * 
	 * @return the amount of records retrieved
	 */
	public int count() {
		final SelectQuery query = this.getQuery();
		if (query.hasLimit()) {
			final int limit = query.getLimit();
			final int offset = query.getOffset();

			if (limit == 0) {
				return 0;
			} else {
				final int recordCount = recordsBitmap.determineCardinality();
				final int availableCount = recordCount - offset;

				if (availableCount < 0) {
					return 0;
				} else if (limit < 0 || limit > availableCount) {
					return availableCount;
				} else {
					return limit;
				}
			}
		} else {
			return recordsBitmap.determineCardinality();
		}
	}
}
