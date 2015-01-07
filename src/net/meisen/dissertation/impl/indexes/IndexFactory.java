package net.meisen.dissertation.impl.indexes;

import java.util.HashMap;

import net.meisen.dissertation.exceptions.IndexFactoryException;
import net.meisen.dissertation.model.indexes.BaseIndexFactory;
import net.meisen.dissertation.model.indexes.IIndexFactoryConfig;
import net.meisen.dissertation.model.indexes.IIndexedCollection;
import net.meisen.dissertation.model.indexes.IMultipleKeySupport;
import net.meisen.dissertation.model.indexes.IPrefixKeySeparatable;
import net.meisen.dissertation.model.indexes.IRangeQueryOptimized;
import net.meisen.dissertation.model.indexes.IndexKeyDefinition;
import net.meisen.dissertation.model.indexes.IndexedCollectionDefinition;
import net.meisen.dissertation.model.indexes.datarecord.slices.Bitmap;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * Factory to create a {@code BaseIndexedCollection}.
 * 
 * @author pmeisen
 * 
 */
public class IndexFactory extends BaseIndexFactory {
	private final static Logger LOG = LoggerFactory
			.getLogger(IndexFactory.class);

	private IndexFactoryConfig config;

	@Override
	public void setConfig(final IIndexFactoryConfig config) {

		// check if we have a configuration already
		if (this.config != null) {
			if (LOG.isWarnEnabled()) {
				LOG.warn("The configuration of the factory is already defined, and cannot be changed.");
			}
		}

		if (config == null) {
			this.config = new IndexFactoryConfig();
		} else if (config instanceof IndexFactoryConfig == false) {
			if (exceptionRegistry == null) {
				throw new IllegalArgumentException(
						"The configuration used for '"
								+ getClass().getSimpleName()
								+ "' must be of the type '"
								+ IndexFactoryConfig.class.getName() + "'.");
			} else {
				exceptionRegistry.throwException(IndexFactoryException.class,
						1003, IndexFactoryConfig.class.getName());
			}
		} else {
			this.config = (IndexFactoryConfig) config;
		}

		if (LOG.isDebugEnabled()) {
			LOG.debug("Setting the '" + getClass().getName()
					+ "' with configuration " + config);
		}
	}

	/**
	 * Gets the configuration of the factory.
	 * 
	 * @return the configuration of the factory
	 */
	public IIndexFactoryConfig getConfig() {
		return getConfiguration();
	}

	/**
	 * Gets the specified configuration. A default configuration is created, if
	 * none is defined so far. Therefore this method will never return
	 * {@code null}.
	 * 
	 * @return the configuration used by the {@code IndexFactory}
	 */
	public IndexFactoryConfig getConfiguration() {
		if (config == null) {
			this.config = new IndexFactoryConfig();

			if (LOG.isInfoEnabled()) {
				LOG.info("Using default configuration " + this.config);
			}
		}

		return this.config;
	}

	@Override
	public IRangeQueryOptimized createRangeQueryOptimized(
			final IndexKeyDefinition keyDef) {
		final int keySize = keyDef.getSize();
		if (keySize != 1) {
			exceptionRegistry.throwRuntimeException(
					IndexFactoryException.class, 1001);
		}

		final Class<?> clazz = keyDef.getType(0);
		if (isBytePrimitiveType(clazz)) {
			return new IntArrayCollection(keyDef);
		} else if (isShortPrimitiveType(clazz)) {
			return new IntArrayCollection(keyDef);
		} else if (isIntPrimitiveType(clazz)) {
			return new IntArrayCollection(keyDef);
		} else {
			exceptionRegistry.throwRuntimeException(
					IndexFactoryException.class, 1002, clazz == null ? null
							: clazz.getName());
			return null;
		}
	}

	@Override
	protected IMultipleKeySupport createMultipleKeySupport(
			final IndexKeyDefinition[] keyDefs,
			final IndexedCollectionDefinition[] collDefs) {
		return new MultipleIndexedCollection(keyDefs, collDefs);
	}

	@Override
	protected IPrefixKeySeparatable createPrefixSeparatable(
			final IndexKeyDefinition keyDef,
			final IndexedCollectionDefinition[] collDefs) {
		return new NestedIndexedCollection(keyDef, collDefs);
	}

	@Override
	protected IndexedCollectionDefinition createIndexedCollectionDefinition(
			final IndexKeyDefinition keyDef) {
		if (keyDef.getSize() <= 1) {
			return createIndexedCollectionDefinition(keyDef.getType(0));
		} else {
			return new IndexedCollectionDefinition(MapIndexedCollection.class,
					HashMap.class);
		}
	}

	@Override
	protected IndexedCollectionDefinition createIndexedCollectionDefinition(
			final Class<?> clazz) {
		final Class<? extends IIndexedCollection> collClazz;

		// we have some really nice implementations for the primitives
		if (isBytePrimitiveType(clazz)) {
			collClazz = getConfiguration().getByteClass();
		} else if (isShortPrimitiveType(clazz)) {
			collClazz = getConfiguration().getShortClass();
		} else if (isIntPrimitiveType(clazz)) {
			collClazz = getConfiguration().getIntClass();
		} else if (isLongPrimitiveType(clazz)) {
			collClazz = getConfiguration().getLongClass();
		} else {
			collClazz = null;
		}

		// if it's not defined yet we use a simple HashMap
		if (collClazz == null) {
			return new IndexedCollectionDefinition(MapIndexedCollection.class,
					HashMap.class);
		} else {
			return new IndexedCollectionDefinition(collClazz);
		}
	}

	@Override
	public Bitmap createBitmap() {

		final Class<? extends Bitmap> bitmapClazz = getConfiguration()
				.getBitmapClass();
		try {
			return bitmapClazz.newInstance();
		} catch (final Exception e) {
			exceptionRegistry.throwRuntimeException(
					IndexFactoryException.class, 1000, e,
					bitmapClazz == null ? null : bitmapClazz.getName());
			return null;
		}
	}
}