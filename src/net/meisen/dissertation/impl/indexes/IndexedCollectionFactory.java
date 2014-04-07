package net.meisen.dissertation.impl.indexes;

import java.util.HashMap;

import net.meisen.dissertation.exceptions.IndexedCollectionFactoryException;
import net.meisen.dissertation.model.indexes.BaseIndexedCollectionFactory;
import net.meisen.dissertation.model.indexes.IMultipleKeySupport;
import net.meisen.dissertation.model.indexes.IPrefixKeySeparatable;
import net.meisen.dissertation.model.indexes.IRangeQueryOptimized;
import net.meisen.dissertation.model.indexes.IndexKeyDefinition;
import net.meisen.dissertation.model.indexes.IndexedCollection;
import net.meisen.dissertation.model.indexes.IndexedCollectionDefinition;
import net.meisen.dissertation.model.indexes.datarecord.bitmap.Bitmap;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * Factory to create a {@code IndexedCollection}.
 * 
 * @author pmeisen
 * 
 */
public class IndexedCollectionFactory extends BaseIndexedCollectionFactory {
	private final static Logger LOG = LoggerFactory
			.getLogger(IndexedCollectionFactory.class);

	private IndexedCollectionFactoryConfig config;

	@Override
	public void setConfig(final Object config) {

		// check if we have a configuration already
		if (this.config != null) {
			if (LOG.isWarnEnabled()) {
				LOG.warn("The configuration of the factory is already defined, but changed.");
			}
		}

		if (config == null) {
			this.config = new IndexedCollectionFactoryConfig();
		} else if (config instanceof IndexedCollectionFactoryConfig == false) {
			if (exceptionRegistry == null) {
				throw new IllegalArgumentException(
						"The configuration used for '"
								+ getClass().getSimpleName()
								+ "' must be of the type '"
								+ IndexedCollectionFactoryConfig.class
										.getName() + "'.");
			} else {
				exceptionRegistry.throwException(
						IndexedCollectionFactoryException.class, 1003,
						IndexedCollectionFactoryConfig.class.getName());
			}
		} else {
			this.config = (IndexedCollectionFactoryConfig) config;
		}

		if (LOG.isDebugEnabled()) {
			LOG.debug("Setting the indexedCollectionFactory with configuration "
					+ config);
		}
	}

	public void setConfig(final IndexedCollectionFactoryConfig config) {
		setConfig((Object) config);
	}

	public IndexedCollectionFactoryConfig getConfig() {
		if (config == null) {
			this.config = new IndexedCollectionFactoryConfig();

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
					IndexedCollectionFactoryException.class, 1001);
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
					IndexedCollectionFactoryException.class, 1002,
					clazz == null ? null : clazz.getName());
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
		final Class<? extends IndexedCollection> collClazz;

		// we have some really nice implementations for the primitives
		if (isBytePrimitiveType(clazz)) {
			collClazz = getConfig().getByteClass();
		} else if (isShortPrimitiveType(clazz)) {
			collClazz = getConfig().getShortClass();
		} else if (isIntPrimitiveType(clazz)) {
			collClazz = getConfig().getIntClass();
		} else if (isLongPrimitiveType(clazz)) {
			collClazz = getConfig().getLongClass();
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

		final Class<? extends Bitmap> bitmapClazz = getConfig()
				.getBitmapClass();
		try {
			return bitmapClazz.newInstance();
		} catch (final Exception e) {
			exceptionRegistry.throwRuntimeException(
					IndexedCollectionFactoryException.class, 1000,
					bitmapClazz == null ? null : bitmapClazz.getName());
			return null;
		}
	}
}