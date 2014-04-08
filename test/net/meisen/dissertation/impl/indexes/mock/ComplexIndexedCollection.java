package net.meisen.dissertation.impl.indexes.mock;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

import net.meisen.dissertation.model.indexes.IndexKeyDefinition;
import net.meisen.dissertation.model.indexes.BaseIndexedCollection;

/**
 * A complex mock of a {@code BaseIndexedCollection}. Complex because it has several
 * constructors.
 * 
 * @author pmeisen
 * 
 */
public class ComplexIndexedCollection extends BaseIndexedCollection {
	private List<Object> params = new ArrayList<Object>();

	/**
	 * Even complex has a simple constructor.
	 * 
	 * @param keyDefinition
	 *            the {@code IndexKeyDefinition}
	 */
	public ComplexIndexedCollection(final IndexKeyDefinition keyDefinition) {
		super(keyDefinition);

		params.add(keyDefinition);
	}

	/**
	 * A constructor which gets 3 values.
	 * 
	 * @param nr
	 *            some number
	 * @param keyDefinition
	 *            the {@code IndexKeyDefinition}
	 * @param someString
	 *            some string
	 * @param whatever
	 *            some object
	 */
	public ComplexIndexedCollection(final Number nr,
			final IndexKeyDefinition keyDefinition, final String someString,
			final Object whatever) {
		super(keyDefinition);

		params.add(nr);
		params.add(keyDefinition);
		params.add(someString);
		params.add(whatever);
	}

	/**
	 * Constructor which one additional parameter.
	 * 
	 * @param keyDefinition
	 *            the {@code IndexKeyDefinition}
	 * @param someString
	 *            some string
	 */
	public ComplexIndexedCollection(final IndexKeyDefinition keyDefinition,
			final String someString) {
		this(keyDefinition, (Object) (someString + "MOD"));
	}

	/**
	 * Another constructor which one additional parameter.
	 * 
	 * @param keyDefinition
	 *            the {@code IndexKeyDefinition}
	 * @param someObject
	 *            some object
	 */
	public ComplexIndexedCollection(final IndexKeyDefinition keyDefinition,
			final Object someObject) {
		super(keyDefinition);

		params.add(keyDefinition);
		params.add(someObject);
	}

	/**
	 * Another constructor which one additional parameter.
	 * 
	 * @param someObject
	 *            some object
	 * @param keyDefinition
	 *            the {@code IndexKeyDefinition}
	 */
	public ComplexIndexedCollection(final Object someObject,
			final IndexKeyDefinition keyDefinition) {
		super(keyDefinition);

		params.add(someObject);
		params.add(keyDefinition);
	}

	/**
	 * Another constructor which has a generalized {@code Number} as parameter
	 * 
	 * @param marker
	 *            some marker to be sure it's this constructor called in the
	 *            test
	 * @param keyDefinition
	 *            the {@code IndexKeyDefinition}
	 * @param nr
	 *            the generalized {@code Number}
	 */
	public ComplexIndexedCollection(final Integer marker,
			final IndexKeyDefinition keyDefinition, final Number nr) {
		super(keyDefinition);

		params.add(keyDefinition);
		params.add(Number.class);
	}

	/**
	 * Another constructor which has a specified {@code Integer} as parameter
	 * 
	 * @param marker
	 *            some marker to be sure it's this constructor called in the
	 *            test
	 * @param keyDefinition
	 *            the {@code IndexKeyDefinition}
	 * @param nr
	 *            the generalized {@code Integer}
	 */
	public ComplexIndexedCollection(final Integer marker,
			final IndexKeyDefinition keyDefinition, final Integer nr) {
		super(keyDefinition);

		params.add(keyDefinition);
		params.add(Integer.class);
	}

	/**
	 * Get the parameters passed to the constructor.
	 * 
	 * @return the parameters passed to the constructor
	 */
	public List<Object> getParameter() {
		return params;
	}

	@Override
	public boolean containsObject(final Object object) {
		return false;
	}

	@Override
	public boolean addObject(final Object object) {
		return false;
	}

	@Override
	public void removeAll() {
		// nothing to do just a mock
	}

	@Override
	public Object getObject(final Object... keys) {
		return null;
	}

	@Override
	public Collection<Object> getAll() {
		return null;
	}

	@Override
	public void removeObject(Object object) {
		// nothing to do just a mock
	}

	@Override
	public int size() {
		return 0;
	}
}
