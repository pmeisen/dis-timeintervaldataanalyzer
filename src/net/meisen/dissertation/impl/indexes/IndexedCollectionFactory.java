package net.meisen.dissertation.impl.indexes;

import java.util.HashMap;

import net.meisen.dissertation.model.indexes.BaseIndexedCollectionFactory;
import net.meisen.dissertation.model.indexes.IMultipleKeySupport;
import net.meisen.dissertation.model.indexes.IPrefixKeySeparatable;
import net.meisen.dissertation.model.indexes.IRangeQueryOptimized;
import net.meisen.dissertation.model.indexes.IndexKeyDefinition;
import net.meisen.dissertation.model.indexes.IndexedCollection;
import net.meisen.dissertation.model.indexes.IndexedCollectionDefinition;

/**
 * Factory to create a {@code IndexedCollection}.
 * 
 * @author pmeisen
 * 
 */
public class IndexedCollectionFactory extends BaseIndexedCollectionFactory {

	@Override
	public IRangeQueryOptimized createRangeQueryOptimized(
			final IndexKeyDefinition keyDef) {
		final int keySize = keyDef.getSize();
		if (keySize != 1) {
			throw new UnsupportedOperationException(
					"A rangeQueryOptimized-indexes doesn't support multiple key values.");
		}

		final Class<?> clazz = keyDef.getType(0);
		if (isBytePrimitiveType(clazz)) {
			return new IntArrayCollection(keyDef);
		} else if (isShortPrimitiveType(clazz)) {
			return new IntArrayCollection(keyDef);
		} else if (isIntPrimitiveType(clazz)) {
			return new IntArrayCollection(keyDef);
		} else {
			throw new UnsupportedOperationException(
					"Cannot find any implementation for rangeQueryOptimized-indexes and '"
							+ clazz + "' values.");
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
			collClazz = TroveByteIndexedCollection.class;
		} else if (isShortPrimitiveType(clazz)) {
			collClazz = TroveShortIndexedCollection.class;
		} else if (isIntPrimitiveType(clazz)) {
			collClazz = TroveIntIndexedCollection.class;
		} else if (isLongPrimitiveType(clazz)) {
			collClazz = TroveLongIndexedCollection.class;
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
}