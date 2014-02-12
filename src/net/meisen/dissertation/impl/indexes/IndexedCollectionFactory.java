package net.meisen.dissertation.impl.indexes;

import java.util.HashMap;

import net.meisen.dissertation.model.indexes.BaseIndexedCollectionFactory;
import net.meisen.dissertation.model.indexes.IMultipleKeySupport;
import net.meisen.dissertation.model.indexes.IPrefixKeySeparatable;
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
	public IPrefixKeySeparatable createPrefixSeparatable(
			final IndexKeyDefinition keyDef,
			final IndexedCollectionDefinition[] collDefs) {
		return new NestedIndexedCollection(keyDef, collDefs);
	}

	@Override
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

		return new MultipleIndexedCollection(keyDefs, collDefs);
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