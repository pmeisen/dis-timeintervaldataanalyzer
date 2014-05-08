package net.meisen.dissertation.model.indexes;

import net.meisen.dissertation.config.xslt.DefaultValues;
import net.meisen.dissertation.model.indexes.datarecord.slices.Bitmap;
import net.meisen.general.genmisc.exceptions.registry.IExceptionRegistry;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;

/**
 * Factory to create a {@code BaseIndexedCollection}.
 * 
 * @author pmeisen
 * 
 */
public abstract class BaseIndexFactory {
	private final static Logger LOG = LoggerFactory
			.getLogger(BaseIndexFactory.class);

	/**
	 * {@code exceptionRegistry} used to fire exceptions.
	 */
	@Autowired
	@Qualifier(DefaultValues.EXCEPTIONREGISTRY_ID)
	protected IExceptionRegistry exceptionRegistry;

	/**
	 * Creates a {@code BaseIndexedCollection} of type
	 * {@code IIndexedCollection}.
	 * 
	 * @param keyDef
	 *            the {@code IndexKeyDefinition} to create the
	 *            {@code BaseIndexedCollection} for
	 * 
	 * @return the created {@code BaseIndexedCollection}
	 */
	public IIndexedCollection create(final IndexKeyDefinition keyDef) {
		return create(keyDef, IIndexedCollection.class);
	}

	/**
	 * Creates a {@code BaseIndexedCollection} of the specified {@code expected}
	 * class. If {@code expected} is {@code null} a
	 * {@code BaseIndexedCollection} of type {@code IIndexedCollection} will be
	 * created (i.e. {@code create(keyDef, IIndexedCollection.class)}.
	 * 
	 * @param keyDef
	 *            the {@code IndexKeyDefinition} to create the
	 *            {@code BaseIndexedCollection} for
	 * @param expected
	 *            the type of the {@code BaseIndexedCollection}, if not
	 *            supported an exception is thrown
	 * 
	 * @return the created {@code BaseIndexedCollection}
	 * 
	 * @throws IllegalArgumentException
	 *             if no {@code BaseIndexedCollection} can be created as defined
	 *             by {@code expected}
	 */
	@SuppressWarnings("unchecked")
	public <T extends IIndexedCollection> T create(
			final IndexKeyDefinition keyDef, final Class<T> expected) {

		if (expected == null || IIndexedCollection.class.equals(expected)) {
			final IndexedCollectionDefinition collDef = createIndexedCollectionDefinition(keyDef);
			return (T) collDef.create(keyDef);
		} else if (IMultipleKeySupport.class.equals(expected)) {
			return (T) createMultipleKeySupport(new IndexKeyDefinition[] { keyDef });
		} else if (IPrefixKeySeparatable.class.equals(expected)) {
			return (T) createPrefixSeparatable(keyDef);
		} else if (IRangeQueryOptimized.class.equals(expected)) {
			return (T) createRangeQueryOptimized(keyDef);
		} else {
			throw new IllegalArgumentException("Cannot create an instance of '"
					+ expected.getName()
					+ "', because it is not supported by this factory.");
		}
	}

	/**
	 * Creates a {@code PrefixKeySeparatable} instance based on the specified
	 * {@code IndexKeyDefinition}.
	 * 
	 * @param keyDef
	 *            the {@code IndexKeyDefinitions} defining the key
	 * 
	 * 
	 * @return the {@code PrefixKeySeparatable} index
	 */
	public IPrefixKeySeparatable createPrefixSeparatable(
			final IndexKeyDefinition keyDef) {
		final int keySize = keyDef.getSize();
		final IndexedCollectionDefinition[] collDefs = new IndexedCollectionDefinition[keySize];
		for (int i = 0; i < keySize; i++) {
			final Class<?> clazz = keyDef.getType(i);
			collDefs[i] = createIndexedCollectionDefinition(clazz);
		}

		return createPrefixSeparatable(keyDef, collDefs);
	}

	/**
	 * Creates an instance of a {@code BaseIndexedCollection} with
	 * {@code MultipleKeySupport} for the specified {@code keyDefs}.
	 * 
	 * @param keyDefs
	 *            the {@code IndexKeyDefinitions} defining the different keys
	 * 
	 * @return the created {@code BaseIndexedCollection} with
	 *         {@code MultipleKeySupport}
	 * 
	 * @see IIndexedCollection
	 * @see IMultipleKeySupport
	 */
	public IMultipleKeySupport createMultipleKeySupport(
			final IndexKeyDefinition... keyDefs) {

		// determine the best index for each key defined
		final int keySize = keyDefs.length;
		final IndexedCollectionDefinition[] collDefs = new IndexedCollectionDefinition[keySize];
		for (int i = 0; i < keySize; i++) {
			final IndexKeyDefinition keyDef = keyDefs[i];
			final IndexedCollectionDefinition collDef = createIndexedCollectionDefinition(keyDef);
			collDefs[i] = collDef;
		}

		return createMultipleKeySupport(keyDefs, collDefs);
	}

	/**
	 * Creates an index which is optimized for range queries for the specified
	 * {@code IndexKeyDefinition}.
	 * 
	 * @param keyDef
	 *            the {@code IndexKeyDefinition} to create the
	 *            {@code IRangeQueryOptimized} index for
	 * 
	 * @return the created index
	 */
	public abstract IRangeQueryOptimized createRangeQueryOptimized(
			final IndexKeyDefinition keyDef);

	/**
	 * Creates an empty, i.e. no value is set to true, bitmap.
	 * 
	 * @return the created bitmap
	 */
	public abstract Bitmap createBitmap();

	/**
	 * Creates a {@code PrefixKeySeparatable} instance based on the specified
	 * {@code IndexKeyDefinition} and the {@code IndexedCollectionDefinition}.
	 * 
	 * @param keyDef
	 *            the {@code IndexKeyDefinitions} defining the key
	 * @param collDefs
	 *            the {@code IndexedCollectionDefinition}
	 * 
	 * @return the {@code PrefixKeySeparatable} index
	 */
	protected abstract IPrefixKeySeparatable createPrefixSeparatable(
			final IndexKeyDefinition keyDef,
			final IndexedCollectionDefinition[] collDefs);

	/**
	 * Creates an instance of a {@code BaseIndexedCollection} with
	 * {@code MultipleKeySupport} for the specified {@code keyDefs} and the best
	 * {@code collDefs}.
	 * 
	 * @param keyDefs
	 *            the {@code IndexKeyDefinitions} defining the different keys
	 * @param collDefs
	 *            the {@code IndexedCollectionDefinition}
	 * 
	 * @return the created {@code BaseIndexedCollection} with
	 *         {@code MultipleKeySupport}
	 * 
	 * @see IIndexedCollection
	 * @see IMultipleKeySupport
	 */
	protected abstract IMultipleKeySupport createMultipleKeySupport(
			final IndexKeyDefinition[] keyDefs,
			final IndexedCollectionDefinition[] collDefs);

	/**
	 * Determines which {@code BaseIndexedCollection} to be used for the
	 * specified {@code keyDef}.
	 * 
	 * @param keyDef
	 *            the {@code IndexKeyDefinition} to determine the
	 *            {@code BaseIndexedCollection} for
	 * 
	 * @return a {@code IndexedCollectionDefinition} which defines the
	 *         {@code BaseIndexedCollection} to be used
	 * 
	 * @see IndexedCollectionDefinition
	 * @see IIndexedCollection
	 * @see IndexKeyDefinition
	 */
	protected abstract IndexedCollectionDefinition createIndexedCollectionDefinition(
			final IndexKeyDefinition keyDef);

	/**
	 * Determines the {@code BaseIndexedCollection} to be used for the specified
	 * {@code clazz}. This method searches from the default
	 * {@code BaseIndexedCollection}, the best fitting once for the specified
	 * index-values (i.e. the class of the index-class).
	 * 
	 * @param clazz
	 *            the index-class to get the {@code BaseIndexedCollection} for
	 * 
	 * @return a {@code IndexedCollectionDefinition} which defines the
	 *         {@code BaseIndexedCollection} to be used
	 * 
	 * @see IndexedCollectionDefinition
	 * @see IIndexedCollection
	 */
	protected abstract IndexedCollectionDefinition createIndexedCollectionDefinition(
			final Class<?> clazz);

	/**
	 * Checks if the specified {@code clazz} is a primitive type.
	 * 
	 * @param clazz
	 *            the class to be checked
	 * 
	 * @return {@code true} if the specified {@code clazz} represents a
	 *         primitive one
	 */
	protected boolean isPrimitiveType(final Class<?> clazz) {

		if (isBytePrimitiveType(clazz)) {
			return true;
		} else if (isShortPrimitiveType(clazz)) {
			return true;
		} else if (isIntPrimitiveType(clazz)) {
			return true;
		} else if (isLongPrimitiveType(clazz)) {
			return true;
		} else if (isFloatPrimitiveType(clazz)) {
			return true;
		} else if (isDoublePrimitiveType(clazz)) {
			return true;
		} else if (isBooleanPrimitiveType(clazz)) {
			return true;
		} else if (isCharPrimitiveType(clazz)) {
			return true;
		} else {
			return false;
		}
	}

	/**
	 * Checks if the specified {@code clazz} is {@code Byte} or {@code byte}.
	 * 
	 * @param clazz
	 *            the class to be checked
	 * 
	 * @return {@code true} if the specified {@code clazz} of type {@code Byte}
	 *         or {@code byte}, otherwise {@code false}
	 */
	protected boolean isBytePrimitiveType(final Class<?> clazz) {
		if (byte.class.equals(clazz) || Byte.class.equals(clazz)) {
			return true;
		} else {
			return false;
		}
	}

	/**
	 * Checks if the specified {@code clazz} is {@code Short} or {@code short}.
	 * 
	 * @param clazz
	 *            the class to be checked
	 * 
	 * @return {@code true} if the specified {@code clazz} of type {@code Short}
	 *         or {@code short}, otherwise {@code false}
	 */
	protected boolean isShortPrimitiveType(final Class<?> clazz) {
		if (short.class.equals(clazz) || Short.class.equals(clazz)) {
			return true;
		} else {
			return false;
		}
	}

	/**
	 * Checks if the specified {@code clazz} is {@code Integer} or {@code int}.
	 * 
	 * @param clazz
	 *            the class to be checked
	 * 
	 * @return {@code true} if the specified {@code clazz} of type
	 *         {@code Integer} or {@code int}, otherwise {@code false}
	 */
	protected boolean isIntPrimitiveType(final Class<?> clazz) {
		if (int.class.equals(clazz) || Integer.class.equals(clazz)) {
			return true;
		} else {
			return false;
		}
	}

	/**
	 * Checks if the specified {@code clazz} is {@code Long} or {@code long}.
	 * 
	 * @param clazz
	 *            the class to be checked
	 * 
	 * @return {@code true} if the specified {@code clazz} of type {@code Long}
	 *         or {@code long}, otherwise {@code false}
	 */
	protected boolean isLongPrimitiveType(final Class<?> clazz) {
		if (long.class.equals(clazz) || Long.class.equals(clazz)) {
			return true;
		} else {
			return false;
		}
	}

	/**
	 * Checks if the specified {@code clazz} is {@code Float} or {@code float}.
	 * 
	 * @param clazz
	 *            the class to be checked
	 * 
	 * @return {@code true} if the specified {@code clazz} of type {@code Float}
	 *         or {@code float}, otherwise {@code false}
	 */
	protected boolean isFloatPrimitiveType(final Class<?> clazz) {
		if (float.class.equals(clazz) || Float.class.equals(clazz)) {
			return true;
		} else {
			return false;
		}
	}

	/**
	 * Checks if the specified {@code clazz} is {@code Double} or {@code double}
	 * .
	 * 
	 * @param clazz
	 *            the class to be checked
	 * 
	 * @return {@code true} if the specified {@code clazz} of type
	 *         {@code Double} or {@code double}, otherwise {@code false}
	 */
	protected boolean isDoublePrimitiveType(final Class<?> clazz) {
		if (double.class.equals(clazz) || Double.class.equals(clazz)) {
			return true;
		} else {
			return false;
		}
	}

	/**
	 * Checks if the specified {@code clazz} is {@code Boolean} or
	 * {@code boolean}.
	 * 
	 * @param clazz
	 *            the class to be checked
	 * 
	 * @return {@code true} if the specified {@code clazz} of type
	 *         {@code Boolean} or {@code boolean}, otherwise {@code false}
	 */
	protected boolean isBooleanPrimitiveType(final Class<?> clazz) {
		if (boolean.class.equals(clazz) || Boolean.class.equals(clazz)) {
			return true;
		} else {
			return false;
		}
	}

	/**
	 * Checks if the specified {@code clazz} is {@code Character} or
	 * {@code char}.
	 * 
	 * @param clazz
	 *            the class to be checked
	 * 
	 * @return {@code true} if the specified {@code clazz} of type
	 *         {@code Character} or {@code char}, otherwise {@code false}
	 */
	protected boolean isCharPrimitiveType(final Class<?> clazz) {
		if (char.class.equals(clazz) || Character.class.equals(clazz)) {
			return true;
		} else {
			return false;
		}
	}

	/**
	 * The base implementation does not support any configuration. Therefore the
	 * default implementation does nothing. If a configuration is supported this
	 * method should be overwritten.
	 * 
	 * @param config
	 *            the configuration to be used
	 */
	public void setConfig(final IIndexFactoryConfig config) {
		// nothing to do this might be implemented by the concrete class
		if (config != null) {
			if (LOG.isInfoEnabled()) {
				LOG.info("A configuration '"
						+ config
						+ "' was passed to the implementation '"
						+ getClass().getName()
						+ "' of an IndexFactory. The configuration is not used, because the base implementation does not support any configuration. Please override the appropriate method in the concrete implementation.");
			}
		}
	}
}
