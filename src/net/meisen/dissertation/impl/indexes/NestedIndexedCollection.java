package net.meisen.dissertation.impl.indexes;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

import net.meisen.dissertation.model.indexes.IPrefixKeySeparatable;
import net.meisen.dissertation.model.indexes.IndexKey;
import net.meisen.dissertation.model.indexes.IndexKeyDefinition;
import net.meisen.dissertation.model.indexes.BaseIndexedCollection;
import net.meisen.dissertation.model.indexes.IndexedCollectionDefinition;
import net.meisen.general.genmisc.types.Objects;

/**
 * A {@code NestedIndexedCollection} is a {@code BaseIndexedCollection} which uses
 * several other {@code BaseIndexedCollection} to index an object (i.e each value of
 * a multiple key is resolved to an own {@code BaseIndexedCollection}.
 * 
 * @author pmeisen
 * 
 */
public class NestedIndexedCollection extends BaseIndexedCollection implements
		IPrefixKeySeparatable {

	private final IndexedCollectionDefinition[] definitions;
	private final BaseIndexedCollection root;

	/**
	 * Helper to put an object and it's identifier in one instance.
	 * 
	 * @author pmeisen
	 * 
	 * @param <T>
	 *            the type of the wrapped object
	 */
	public static class Wrapper<T> {
		private final Object id;
		private final T wrapped;

		/**
		 * Constructor to create a {@code Wrapper} with it's {@code id} and the
		 * item to be wrapped.
		 * 
		 * @param id
		 *            the identifier for the wrapped object
		 * @param wrapped
		 *            the item to be wrapped
		 */
		public Wrapper(final Object id, final T wrapped) {
			this.id = id;
			this.wrapped = wrapped;
		}

		/**
		 * Gets the identifier of the wrapped object.
		 * 
		 * @return identifier of the wrapped object
		 */
		public Object getId() {
			return id;
		}

		/**
		 * Gets the object associated to the identifier.
		 * 
		 * @return the object associated to the identifier
		 */
		public T get() {
			return wrapped;
		}

		@Override
		public int hashCode() {
			return wrapped == null ? 0 : wrapped.hashCode();
		}

		@Override
		public boolean equals(final Object o) {
			return Objects.equals(wrapped, o);
		}
	}

	/**
	 * Constructor to pass the {@code IndexKeyDefinition} and the definitions of
	 * the {@code BaseIndexedCollection} nested.
	 * 
	 * @param keyDefinition
	 *            the {@code IndexKeyDefinition} to define the key of this
	 *            collection
	 * @param definitions
	 *            the {@code NestedCollectionDefinition} instances of the
	 *            {@code IndexKeyDefinition} instances nested
	 */
	public NestedIndexedCollection(final IndexKeyDefinition keyDefinition,
			final IndexedCollectionDefinition... definitions) {
		super(keyDefinition);

		if (definitions == null) {
			throw new NullPointerException("The definitions cannot be null.");
		} else if (keyDefinition.getSize() != definitions.length) {
			throw new IllegalArgumentException(
					"The size of the keyDefinition '"
							+ keyDefinition.getSize()
							+ "' (i.e. the different parts of the key) must be equal to the amount of NestedCollectionDefinitions '"
							+ definitions.length + "' ('" + keyDefinition
							+ "').");
		} else if (definitions.length == 0) {
			throw new IllegalArgumentException(
					"There must be at least one definition defined.");
		}

		this.definitions = definitions;
		this.root = create(null, 0).get();
	}

	@Override
	public boolean containsObject(final Object object) {
		final IndexKey<?> key = getKeyDefinition().getKey(object);
		return getObject(key.getValues()) != null;
	}

	@Override
	public boolean addObject(final Object object) {
		final IndexKey<?> key = getKeyDefinition().getKey(object);
		final Object[] values = key.getValues();

		final BaseIndexedCollection collection = getCollection(values, true);
		final Object lastValue = values[values.length - 1];

		return collection.addObject(new Wrapper<Object>(lastValue, object));
	}

	@Override
	public void removeObject(final Object object) {
		final IndexKey<?> key = getKeyDefinition().getKey(object);
		final Object[] values = key.getValues();

		final BaseIndexedCollection collection = getCollection(values, false);
		final Object lastValue = values[values.length - 1];

		collection.removeObject(new Wrapper<Object>(lastValue, object));
	}

	/**
	 * Helper method to get the collection addressed by the specified (prefix)
	 * values.
	 * 
	 * @param values
	 *            the values to find the {@code BaseIndexedCollection} defined by,
	 *            can be just some prefix values (e.g. the first four on a six
	 *            sized index)
	 * @param create
	 *            {@code true} if the path to the {@code BaseIndexedCollection}
	 *            should be created, otherwise {@code false}
	 * 
	 * @return the found or created {@code BaseIndexedCollection} or {@code null} if
	 *         it couldn't be found
	 */
	protected BaseIndexedCollection getCollection(final Object[] values,
			final boolean create) {

		BaseIndexedCollection collection = this.root;

		// if we have a full definition we cannot use the last key
		final int lastValue = values.length
				- (values.length == definitions.length ? 1 : 0);
		for (int i = 0; i < lastValue; i++) {
			final Object key = values[i];

			// get the collection behind this value
			@SuppressWarnings("unchecked")
			Wrapper<BaseIndexedCollection> c = (Wrapper<BaseIndexedCollection>) collection
					.getObject(key);

			// create a new collection if there isn't one yet
			if (c == null) {
				if (create) {
					c = create(key, i + 1);
					collection.addObject(c);
				} else {
					collection = null;
					break;
				}
			}

			// c contains the collection we should go on with it
			collection = c.get();
		}

		return collection;
	}

	@Override
	public void removeAll() {
		this.root.removeAll();
	}

	@Override
	public Object getObject(final Object... values) {

		if (values == null) {
			throw new NullPointerException("The values cannot be null.");
		} else if (values.length > definitions.length) {
			throw new IllegalArgumentException(
					"The size of the specified values cannot be more detailed than the definitions size.");
		}

		final BaseIndexedCollection collection = getCollection(values, false);
		if (collection == null) {
			return null;
		} else {
			final Object lastValue = values[values.length - 1];

			@SuppressWarnings("unchecked")
			final Wrapper<Object> wrappedObject = (Wrapper<Object>) collection
					.getObject(lastValue);
			return wrappedObject == null ? null : wrappedObject.get();
		}
	}

	@Override
	public List<Object> getObjects(final Object... values) {

		if (values == null) {
			throw new NullPointerException("The values cannot be null.");
		} else if (values.length > definitions.length) {
			throw new IllegalArgumentException(
					"The size of the specified values cannot be more detailed than the definitions size.");
		}

		final List<Object> l = new ArrayList<Object>();
		if (values.length == definitions.length) {
			final Object o = getObject(values);
			if (o != null) {
				l.add(o);
			}
		} else {
			final BaseIndexedCollection collection = getCollection(values, false);

			// get all the values from all the collections
			if (collection != null) {
				getAll(collection, l);
			}
		}

		return l;
	}

	/**
	 * Helper method which calls itself recursively to get all the elements of
	 * the {@code BaseIndexedCollection} passed. That means that the tree of
	 * {@code BaseIndexedCollection} is resolved down to the leaves and the list
	 * {@code l} is filled with those.
	 * 
	 * @param collection
	 *            the {@code BaseIndexedCollection} to start from
	 * @param l
	 *            the {@code List} to be filled
	 */
	protected void getAll(final BaseIndexedCollection collection,
			final List<Object> l) {
		for (final Object o : collection.getAll()) {
			final Object w = ((Wrapper<?>) o).get();
			if (w instanceof BaseIndexedCollection) {
				getAll((BaseIndexedCollection) w, l);
			} else {
				l.add(w);
			}
		}
	}

	@Override
	public Collection<Object> getAll() {
		final List<Object> l = new ArrayList<Object>();
		getAll(this.root, l);

		return l;
	}

	/**
	 * Creates an instance of the defined {@code BaseIndexedCollection} using the
	 * {@code NestedCollectionDefinition} at position {@code nr}.
	 * 
	 * @param id
	 *            the identifier used to identify the {@code BaseIndexedCollection}
	 * @param nr
	 *            the number of the {@code NestedCollectionDefinition} to be
	 *            used
	 * 
	 * @return an instance of a {@code Wrapper} which is used to pass the
	 *         {@code id} and the created instance
	 */
	protected Wrapper<BaseIndexedCollection> create(final Object id, final int nr) {
		final IndexedCollectionDefinition def = this.definitions[nr];
		final IndexKeyDefinition keyDef = new IndexKeyDefinition(Wrapper.class,
				"getId");
		final BaseIndexedCollection idx = def.create(keyDef);

		return new Wrapper<BaseIndexedCollection>(id, idx);
	}
	
	@Override
	public int size() {
		return getAll().size();
	}
}
