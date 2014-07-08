package net.meisen.dissertation.impl.parser.query.get;

import java.util.HashSet;
import java.util.Iterator;
import java.util.Set;

import net.meisen.dissertation.model.data.FieldNameGenerator;
import net.meisen.dissertation.model.parser.query.IQueryResultSet;

/**
 * A result of a {@code GetQuery}.
 * 
 * @author pmeisen
 * 
 */
public class GetResultModels implements IQueryResultSet {

	private final Set<String> tidaModels;

	/**
	 * Default constructor which creates an empty result.
	 */
	public GetResultModels() {
		this(null);
	}

	/**
	 * Constructor which specifies the {@code tidaModels} of the result.
	 * 
	 * @param tidaModels
	 *            the result, i.e. the models
	 */
	public GetResultModels(final Set<String> tidaModels) {
		if (tidaModels == null) {
			this.tidaModels = new HashSet<String>();
		} else {
			this.tidaModels = tidaModels;
		}
	}

	@Override
	public int[] getCollectedIds() {
		return null;
	}

	@Override
	public Iterator<Object[]> iterator() {

		return new Iterator<Object[]>() {
			private Iterator<String> it = tidaModels.iterator();

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
	}

	@Override
	public Class<?>[] getTypes() {
		return new Class<?>[] { String.class };
	}

	@Override
	public String[] getNames() {
		return new String[] { FieldNameGenerator.get().getModelIdFieldName() };
	}

	/**
	 * Get the amount of models within the result.
	 * 
	 * @return the amount of models
	 */
	public int size() {
		return tidaModels.size();
	}
}
