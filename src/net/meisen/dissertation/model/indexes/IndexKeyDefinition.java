package net.meisen.dissertation.model.indexes;

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;

import net.meisen.dissertation.model.indexes.keys.CompositeIndexKey;
import net.meisen.dissertation.model.indexes.keys.WrappedObjectIndexKey;

/**
 * A definition of an {@code IndexKey}.
 * 
 * @author pmeisen
 * 
 * @see IndexKey
 * 
 */
public class IndexKeyDefinition {

	private final Class<?> objectClass;
	private final Method[] getterMethods;
	private final Class<?>[] getterTypes;
	private final Class<?>[] overridenTypes;
	private final int size;
	private final IndexedCollectionIdResolver<?> idResolver;

	/**
	 * Constructor to create an {@code IndexKey} which defines an object of the
	 * {@code objectClass} as key.
	 * 
	 * @param objectClass
	 *            the type of the key and {@code object}
	 */
	public IndexKeyDefinition(final Class<?> objectClass) {
		this(objectClass, (IndexedCollectionIdResolver<?>) null);
	}

	/**
	 * Constructor to create an {@code IndexKeyDefinition} which defines an
	 * {@code IndexKey} whereby the key is defined by a single value retrieved
	 * from an {@code idResolver} which creates an id based on an object of the
	 * {@code objectClass}.
	 * 
	 * @param objectClass
	 *            the class of which the object must be
	 * @param idResolver
	 *            the {@code idResolver} which resolved the id of an object of
	 *            type {@code objectClass}
	 */
	public IndexKeyDefinition(final Class<?> objectClass,
			final IndexedCollectionIdResolver<?> idResolver) {
		if (objectClass == null) {
			throw new NullPointerException("objectClass cannot be null.");
		}

		this.objectClass = primitiveTypeMapper(objectClass);
		this.getterMethods = null;
		this.getterTypes = null;
		this.overridenTypes = null;
		this.idResolver = idResolver;
		this.size = 0;
	}

	/**
	 * Constructor to create an {@code IndexKeyDefinition} which defines an
	 * {@code IndexKey} based on the methods of an object from a specific
	 * {@code objectClass}.
	 * 
	 * @param objectClass
	 *            the type of the object which should be indexed
	 * @param getterMethods
	 *            the method of the object which define the key
	 */
	public IndexKeyDefinition(final Class<?> objectClass,
			final String... getterMethods) {
		if (objectClass == null) {
			throw new NullPointerException("objectClass cannot be null.");
		} else if (getterMethods == null) {
			throw new NullPointerException("getterMethods cannot be null.");
		} else if (getterMethods.length < 1) {
			throw new IllegalArgumentException(
					"getterMethods must contain at least one method.");
		}

		this.idResolver = null;
		this.objectClass = primitiveTypeMapper(objectClass);
		this.size = getterMethods.length;

		this.getterMethods = new Method[size];
		this.getterTypes = new Class<?>[size];
		this.overridenTypes = new Class<?>[size];
		for (int i = 0; i < size; i++) {
			final String getterMethod = getterMethods[i];
			if (getterMethod == null) {
				throw new IllegalArgumentException("The method '" + null
						+ "' cannot be accessed or found within the class '"
						+ objectClass + "'");
			}

			// determine the method of the class
			final Method m;
			try {
				m = objectClass.getMethod(getterMethod);
				this.getterMethods[i] = m;
			} catch (final NoSuchMethodException e) {
				throw new IllegalArgumentException("The method '"
						+ getterMethod
						+ "' cannot be accessed or found within the class '"
						+ objectClass + "'", e);
			}

			// determine the return type
			final Class<?> returnClass = primitiveTypeMapper(m.getReturnType());
			if (returnClass.equals(Void.TYPE)) {
				throw new IllegalArgumentException(
						"The method '"
								+ m.getName()
								+ "' doesn't return any value. It must be a getter method.");
			}
			this.getterTypes[i] = returnClass;
			this.overridenTypes[i] = null;
		}
	}

	/**
	 * Overrides a specific type of the {@code IndexKeyDefinition}.
	 * 
	 * @param pos
	 * @param type
	 */
	public void overrideType(final int pos, final Class<?> type) {
		if (pos < 0 || pos >= this.size) {
			throw new IllegalArgumentException("Cannot override the position '"
					+ pos + "' please use a position between [0, " + size
					+ ").");
		}

		if (type == null) {
			overridenTypes[pos] = null;
		} else if (getterTypes[pos].isAssignableFrom(type)) {
			overridenTypes[pos] = primitiveTypeMapper(type);
		} else {
			throw new IllegalArgumentException("Cannot override '"
					+ getterTypes[pos].getName() + "' with '" + type.getName()
					+ "'.");
		}
	}

	/**
	 * Get the size of the key, i.e. how many values make up the key. The method
	 * returns {@code 0} if the object is the key itself.
	 * 
	 * @return the size of the key
	 */
	public int getSize() {
		return this.size;
	}

	/**
	 * Get the types of the key.
	 * 
	 * @return the types of the key
	 */
	public Class<?>[] getTypes() {
		final Class<?>[] types = new Class<?>[this.size];
		for (int i = 0; i < this.size; i++) {
			types[i] = getType(i);
		}

		return types;
	}

	/**
	 * Gets the type defined for the specified {@code pos}.
	 * 
	 * @param pos
	 *            the position of the type of the value of the key to be
	 *            returned
	 * 
	 * @return the type of the value of the key at the specified {@code pos}
	 */
	public Class<?> getType(final int pos) {
		Class<?> type;
		if (size == 0) {
			type = idResolver == null ? objectClass : idResolver.getIdType();
		} else if ((type = overridenTypes[pos]) == null) {
			type = getterTypes[pos];
		}

		return type;
	}

	/**
	 * Determines the key for the specified {@code object}.
	 * 
	 * @param object
	 *            the object to determine the key for
	 * 
	 * @return the created {@code IndexKey}
	 * 
	 * @see IndexKey
	 * @see WrappedObjectIndexKey
	 * @see CompositeIndexKey
	 */
	public IndexKey<?> getKey(final Object object) {
		if (object == null) {
			throw new NullPointerException("An indexed object cannot be null.");
		} else if (this.size > 1) {
			return new CompositeIndexKey(getValues(object));
		} else if (this.size < 1) {
			return new WrappedObjectIndexKey(getValue(object, getType(0)));
		} else {
			return new WrappedObjectIndexKey(getValue(object, getterMethods[0]));
		}
	}

	/**
	 * Generates a key from the passed values, i.e. concrete values of a key.
	 * The values are used as is and wrapped into the correct {@code IndexKey}.
	 * Furthermore the passed {@code objects} are matched against the expected
	 * types defined for the {@code IndexKeyDefinition}.
	 * 
	 * @param objects
	 *            the objects which should be handled as key
	 * 
	 * @return the created {@code IndexKey}
	 * 
	 * @see IndexKey
	 * @see WrappedObjectIndexKey
	 * @see CompositeIndexKey
	 */
	public IndexKey<?> generateKeyFromValues(final Object... objects) {
		if (objects == null) {
			throw new NullPointerException("Cannot create a key from null.");
		} else if (this.size < 1) {
			if (objects.length != 1) {
				throw new IllegalArgumentException(
						"Invalid amount of objects passed.");
			} else if (!objectClass.isAssignableFrom(objects[0].getClass())) {
				throw new IllegalArgumentException("The class '"
						+ objectClass.getName()
						+ " cannot be assigned to the passed object '"
						+ objects[0] + "' ('" + objects[0].getClass().getName()
						+ "').");
			}

			return new WrappedObjectIndexKey(objects[0]);
		} else if (objects.length != this.size) {
			throw new IllegalArgumentException(
					"The amount of objects '"
							+ objects.length
							+ "' doesn't match the amount of values needed by this key (i.e. '"
							+ this.size + "')");
		} else if (this.size > 1) {

			// check the types
			for (int i = 0; i < this.size; i++) {
				if (!getType(i).isAssignableFrom(objects[i].getClass())) {
					throw new IllegalArgumentException("The class '"
							+ getType(i).getName()
							+ " cannot be assigned to the passed object '"
							+ objects[i] + "' ('"
							+ objects[i].getClass().getName() + "').");
				}
			}

			return new CompositeIndexKey(objects);
		} else if (objects[0] == null) {
			throw new NullPointerException("Cannot create a key from null.");
		} else {
			if (!getType(0).isAssignableFrom(objects[0].getClass())) {
				throw new IllegalArgumentException("The class '"
						+ getType(0).getName()
						+ " cannot be assigned to the passed object '"
						+ objects[0] + "' ('" + objects[0].getClass().getName()
						+ "').");
			}

			return new WrappedObjectIndexKey(objects[0]);
		}
	}

	/**
	 * Determines a byte key for the passed {@code object}. The method of
	 * determining the key dependency on the definition of the key, e.g. via
	 * method-calling, directly using the passed object or using an
	 * {@code IdResolver}.
	 * 
	 * @param object
	 *            the object to determine the key for
	 * @return the byte key for the {@code object}
	 * 
	 * @see IndexedCollectionIdResolver
	 */
	public byte getByteKey(final Object object) {
		return (Byte) getValue(object, Byte.class);
	}

	/**
	 * Generate a byte key based on the passed {@code object}. The passed
	 * {@code object} must be a {@code byte} or {@code Byte}, otherwise the
	 * passed value (i.e. the {@code object}) cannot be interpreted as such.
	 * 
	 * @param object
	 *            the object to create the byte key for
	 * 
	 * @return a byte key, if one can be created for the specified
	 *         {@code object} otherwise an exception is thrown
	 */
	public byte generateByteKeyFromValue(final Object object) {
		return generateTypedKeyFromValue(object, Byte.class);
	}

	/**
	 * Determines a short key for the passed {@code object}. The method of
	 * determining the key dependency on the definition of the key, e.g. via
	 * method-calling, directly using the passed object or using an
	 * {@code IdResolver}.
	 * 
	 * @param object
	 *            the object to determine the key for
	 * @return the short key for the {@code object}
	 * 
	 * @see IndexedCollectionIdResolver
	 */
	public short getShortKey(final Object object) {
		return (Short) getValue(object, Short.class);
	}

	/**
	 * Generate a short key based on the passed {@code object}. The passed
	 * {@code object} must be a {@code short} or {@code Short}, otherwise the
	 * passed value (i.e. the {@code object}) cannot be interpreted as such.
	 * 
	 * @param object
	 *            the object to create the short key for
	 * 
	 * @return a short key, if one can be created for the specified
	 *         {@code object} otherwise an exception is thrown
	 */
	public short generateShortKeyFromValue(final Object object) {
		return generateTypedKeyFromValue(object, Short.class);
	}

	/**
	 * Determines a int key for the passed {@code object}. The method of
	 * determining the key dependency on the definition of the key, e.g. via
	 * method-calling, directly using the passed object or using an
	 * {@code IdResolver}.
	 * 
	 * @param object
	 *            the object to determine the key for
	 * @return the int key for the {@code object}
	 * 
	 * @see IndexedCollectionIdResolver
	 */
	public int getIntKey(final Object object) {
		return (Integer) getValue(object, Integer.class);
	}

	/**
	 * Generate a int key based on the passed {@code object}. The passed
	 * {@code object} must be a {@code int} or {@code Integer}, otherwise the
	 * passed value (i.e. the {@code object}) cannot be interpreted as such.
	 * 
	 * @param object
	 *            the object to create the int key for
	 * 
	 * @return a int key, if one can be created for the specified {@code object}
	 *         otherwise an exception is thrown
	 */
	public int generateIntKeyFromValue(final Object object) {
		return generateTypedKeyFromValue(object, Integer.class);
	}

	/**
	 * Determines a long key for the passed {@code object}. The method of
	 * determining the key dependency on the definition of the key, e.g. via
	 * method-calling, directly using the passed object or using an
	 * {@code IdResolver}.
	 * 
	 * @param object
	 *            the object to determine the key for
	 * @return the long key for the {@code object}
	 * 
	 * @see IndexedCollectionIdResolver
	 */
	public long getLongKey(final Object object) {
		return (Long) getValue(object, Long.class);
	}

	/**
	 * Generate a long key based on the passed {@code object}. The passed
	 * {@code object} must be a {@code long} or {@code Long}, otherwise the
	 * passed value (i.e. the {@code object}) cannot be interpreted as such.
	 * 
	 * @param object
	 *            the object to create the long key for
	 * 
	 * @return a long key, if one can be created for the specified
	 *         {@code object} otherwise an exception is thrown
	 */
	public long generateLongKeyFromValue(final Object object) {
		return generateTypedKeyFromValue(object, Long.class);
	}

	/**
	 * Determines a float key for the passed {@code object}. The method of
	 * determining the key dependency on the definition of the key, e.g. via
	 * method-calling, directly using the passed object or using an
	 * {@code IdResolver}.
	 * 
	 * @param object
	 *            the object to determine the key for
	 * @return the float key for the {@code object}
	 * 
	 * @see IndexedCollectionIdResolver
	 */
	public float getFloatKey(final Object object) {
		return (Float) getValue(object, Float.class);
	}

	/**
	 * Generate a float key based on the passed {@code object}. The passed
	 * {@code object} must be a {@code float} or {@code Float}, otherwise the
	 * passed value (i.e. the {@code object}) cannot be interpreted as such.
	 * 
	 * @param object
	 *            the object to create the float key for
	 * 
	 * @return a float key, if one can be created for the specified
	 *         {@code object} otherwise an exception is thrown
	 */
	public float generateFloatKeyFromValue(final Object object) {
		return generateTypedKeyFromValue(object, Float.class);
	}

	/**
	 * Determines a char key for the passed {@code object}. The method of
	 * determining the key dependency on the definition of the key, e.g. via
	 * method-calling, directly using the passed object or using an
	 * {@code IdResolver}.
	 * 
	 * @param object
	 *            the object to determine the key for
	 * @return the char key for the {@code object}
	 * 
	 * @see IndexedCollectionIdResolver
	 */
	public char getCharKey(final Object object) {
		return (Character) getValue(object, Character.class);
	}

	/**
	 * Generate a char key based on the passed {@code object}. The passed
	 * {@code object} must be a {@code char} or {@code Character}, otherwise the
	 * passed value (i.e. the {@code object}) cannot be interpreted as such.
	 * 
	 * @param object
	 *            the object to create the char key for
	 * 
	 * @return a char key, if one can be created for the specified
	 *         {@code object} otherwise an exception is thrown
	 */
	public char generateCharKeyFromValue(final Object object) {
		return generateTypedKeyFromValue(object, Character.class);
	}

	/**
	 * Determines a boolean key for the passed {@code object}. The method of
	 * determining the key dependency on the definition of the key, e.g. via
	 * method-calling, directly using the passed object or using an
	 * {@code IdResolver}.
	 * 
	 * @param object
	 *            the object to determine the key for
	 * @return the boolean key for the {@code object}
	 * 
	 * @see IndexedCollectionIdResolver
	 */
	public boolean getBooleanKey(final Object object) {
		return (Boolean) getValue(object, Boolean.class);
	}

	/**
	 * Generate a boolean key based on the passed {@code object}. The passed
	 * {@code object} must be a {@code boolean} or {@code Boolean}, otherwise
	 * the passed value (i.e. the {@code object}) cannot be interpreted as such.
	 * 
	 * @param object
	 *            the object to create the boolean key for
	 * 
	 * @return a boolean key, if one can be created for the specified
	 *         {@code object} otherwise an exception is thrown
	 */
	public boolean generateBooleanKeyFromValue(final Object object) {
		return generateTypedKeyFromValue(object, Boolean.class);
	}

	/**
	 * Generates a key from a value if and only if the defined key is based on
	 * an object (see {@link #IndexKeyDefinition(Class)} ) or a single method
	 * (i.e. calling {@link #IndexKeyDefinition(Class, String...)} with a single
	 * method).
	 * 
	 * @param object
	 *            the value which is the key
	 * @param expected
	 *            the expected type of the key
	 * 
	 * @return the checked object which can be used as key
	 */
	protected <T> T generateTypedKeyFromValue(final Object object,
			final Class<T> expected) {
		final Object result;

		if (object == null) {
			throw new NullPointerException(
					"The object cannot be null if used as long value.");
		} else if (this.size > 1) {
			throw new IllegalStateException(
					"Cannot generate a key from a single value, within an IndexKeyDefinition with multiple keys defined.");
		} else if (object != null
				&& !expected.isAssignableFrom(object.getClass())) {
			throw new IllegalArgumentException("The object '" + object + "' ('"
					+ object.getClass().getName()
					+ "') is not of the expected type '" + expected.getName()
					+ "'.");
		} else if (this.size < 1) {
			result = object;

			// check the result against the expected
			if (!objectClass.isAssignableFrom(expected)) {
				throw new IllegalArgumentException("The expected class '"
						+ expected.getName()
						+ "' is not compatible to the key-class '"
						+ objectClass.getName() + "'.");
			}
		} else {
			result = object;

			if (!getType(0).isAssignableFrom(expected)) {
				throw new IllegalArgumentException("The expected '"
						+ expected.getName()
						+ "' isn't compatible to the key-class '"
						+ getType(0).getName() + "'");
			}
		}

		@SuppressWarnings("unchecked")
		final T typedResult = (T) result;
		return typedResult;
	}

	/**
	 * Checks if the key of this index is defined by only one value.
	 * 
	 * @return {@code true} if the key is specified by exactly one value,
	 *         otherwise {@code false}
	 */
	public boolean isSingleTypedKey() {
		return this.size < 2;
	}

	/**
	 * This method checks if a key consists of only one value. This is the case,
	 * only and only if the object is the key itself or if only one method of
	 * the object specifies the key. Furthermore this method returns
	 * {@code true} only and only if the class of the exactly one value is equal
	 * to the {@code clazz}. <br/>
	 * <br/>
	 * <b>Note:</b><br/>
	 * The {@code class} is mapped using the {@code primitiveTypeMapper(Class)}
	 * prior to comparison. All classes used within a {@code IndexKeyDefinition}
	 * are mapped using the mentioned method.
	 * 
	 * @param clazz
	 *            the class to check the one-value against
	 * 
	 * @return {@code true} if the key is specified by exactly one value and if
	 *         this value is of the specified {@code clazz}
	 */
	public boolean isSingleTypedKey(final Class<?> clazz) {
		if (this.size > 1) {
			return false;
		}

		final Class<?> mappedClazz = primitiveTypeMapper(clazz);
		if (this.size > 0) {
			return getType(0).isAssignableFrom(mappedClazz);
		} else if (idResolver != null) {
			return idResolver.getIdType().isAssignableFrom(mappedClazz);
		} else {
			return objectClass.isAssignableFrom(mappedClazz);
		}
	}

	/**
	 * Gets the key of the specified {@code object}.
	 * 
	 * @param object
	 *            the object to retrieve the key from
	 *            
	 * @return the key of the object
	 */
	public Object getObjectKey(final Object object) {
		return getValue(object, Object.class);
	}

	/**
	 * Get the value from the specified {@code object} if and only if the
	 * defined key is based on an object (see {@link #IndexKeyDefinition(Class)}
	 * ) or a single method (i.e. calling
	 * {@link #IndexKeyDefinition(Class, String...)} with a single method).
	 * 
	 * @param object
	 *            the object to get the value of the {@code expected} type form
	 * @param expected
	 *            the type of the retrieved value
	 * 
	 * @return a value retrieved from the specified {@code object} by using the
	 *         key-definition; the value is of the {@code expected} type
	 */
	protected <T extends Object> T getValue(final Object object,
			final Class<T> expected) {
		final Object result;
		if (this.size > 1) {
			throw new IllegalStateException(
					"Cannot retrieve a value from an object, within an IndexKeyDefinition with multiple keys defined.");
		} else if (this.size < 1) {

			// check if the id is resolved from another resolver
			if (this.idResolver == null) {
				if (object != null
						&& !objectClass.isAssignableFrom(object.getClass())) {
					throw new IllegalArgumentException("The object '" + object
							+ "' ('" + object.getClass().getName()
							+ "') isn't of type '" + objectClass.getName()
							+ "'");
				} else if (!objectClass.isAssignableFrom(expected)) {
					throw new IllegalArgumentException("The expected class '"
							+ expected.getName()
							+ "' is not compatible to the key-class '"
							+ objectClass.getName() + "'.");
				}

				result = object;
			}
			// use the resolver to get the id
			else {
				result = idResolver.getId(object);
			}
		} else {
			result = getValue(object, getterMethods[0]);
		}

		// check the result against the expected
		if (result != null && !expected.isAssignableFrom(result.getClass())) {
			throw new IllegalArgumentException(
					"Expected an object which is assignable from '"
							+ expected.getName() + "' but got '" + result
							+ "' ('" + result.getClass().getName() + "')");
		}

		@SuppressWarnings("unchecked")
		final T typedResult = (T) result;
		return typedResult;
	}

	/**
	 * Gets a value of the passed {@code object} by calling the specified
	 * {@code m}.
	 * 
	 * @param object
	 *            the object to call the method from
	 * @param m
	 *            the method to be called
	 * 
	 * @return the return value of the {@code object} when calling the method
	 *         {@code m}
	 */
	protected Object getValue(final Object object, final Method m) {
		if (object == null) {
			throw new NullPointerException("The object cannot be null.");
		} else if (!objectClass.isAssignableFrom(object.getClass())) {
			throw new IllegalArgumentException("The passed object '"
					+ object.getClass().getName()
					+ "' cannot be assigned by index-class '"
					+ objectClass.getName() + "'.");
		}

		try {
			return m.invoke(object);
		} catch (final IllegalAccessException e) {
			throw new IllegalArgumentException(
					"The method '"
							+ m.getName()
							+ "' could not be executed, because the access is not permitted.",
					e);
		} catch (final InvocationTargetException e) {
			throw new IllegalArgumentException(
					"The method '"
							+ m.getName()
							+ "' could not be executed, because of an inner exception.",
					e);
		}
	}

	/**
	 * Gets the values from the {@code object} which are specified as key by
	 * {@code this} key-definition.
	 * 
	 * @param object
	 *            the {@code object} to get the values of the key from
	 * 
	 * @return the retrieved values from the {@code object}
	 */
	protected Object[] getValues(final Object object) {
		final Object[] values = new Object[this.size];
		for (int i = 0; i < this.size; i++) {
			values[i] = getValue(object, this.getterMethods[i]);
		}

		return values;
	}

	/**
	 * Mapper to map the primitive types to their corresponding
	 * 
	 * @param clazz
	 *            the {@code Class} to be mapped
	 * @return the none-primitive class, if {@code clazz} was primitive one,
	 *         otherwise the {@code Class} which was represented by
	 *         {@code clazz}.
	 */
	protected Class<?> primitiveTypeMapper(final Class<?> clazz) {

		if (byte.class.equals(clazz)) {
			return Byte.class;
		} else if (short.class.equals(clazz)) {
			return Short.class;
		} else if (int.class.equals(clazz)) {
			return Integer.class;
		} else if (long.class.equals(clazz)) {
			return Long.class;
		} else if (float.class.equals(clazz)) {
			return Float.class;
		} else if (double.class.equals(clazz)) {
			return Double.class;
		} else if (boolean.class.equals(clazz)) {
			return Boolean.class;
		} else if (char.class.equals(clazz)) {
			return Character.class;
		} else {
			return clazz;
		}
	}

	@Override
	public String toString() {
		String res = "IndexKeyDefinition for '" + objectClass.getName()
				+ "' indexing ";
		if (this.size > 0) {
			res += this.size + "-method(s):";
			for (final Method m : getterMethods) {
				res += " " + m.getName();
			}
		} else if (idResolver == null) {
			res += "the complete object";
		} else {
			res += "the complete object using a idResolver ('"
					+ idResolver.getClass().getName() + "')";
		}

		return res;
	}

	/**
	 * Returns the class of the object to be indexed.
	 * 
	 * @return the class of hte object to be indexed
	 */
	public Class<?> getObjectClass() {
		return this.objectClass;
	}

	/**
	 * Checks if the specified keys match this definition, i.e. if the specified
	 * keys fulfill the definition.
	 * 
	 * @param keys
	 *            the keys to be checked
	 * 
	 * @return {@code true} if those match, otherwise {@code false}
	 */
	public boolean matches(final Object... keys) {
		if (keys == null) {
			return false;
		} else if (idResolver != null && keys.length != 1) {
			return false;
		} else if (idResolver != null) {
			final Object key = keys[0];
			if (key == null) {
				return true;
			} else {
				final Class<?> mappedClass = primitiveTypeMapper(key.getClass());
				return idResolver.getIdType().isAssignableFrom(mappedClass);
			}
		} else if (this.size == 0 && keys.length != 1) {
			return false;
		} else if (this.size == 0) {
			final Object key = keys[0];
			if (key == null) {
				return true;
			} else {
				final Class<?> mappedClass = primitiveTypeMapper(key.getClass());
				return objectClass.isAssignableFrom(mappedClass);
			}
		} else if (keys.length != this.size) {
			return false;
		} else {

			for (int i = 0; i < this.size; i++) {
				final Object key = keys[i];

				if (key == null) {
					continue;
				}

				final Class<?> mappedClass = primitiveTypeMapper(key.getClass());
				if (!getType(i).isAssignableFrom(mappedClass)) {
					return false;
				}
			}
		}

		return true;
	}
}
