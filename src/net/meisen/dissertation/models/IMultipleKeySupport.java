package net.meisen.dissertation.models;



/**
 * This interface describes the property of an {@code IndexedCollection}, which
 * identifies that multiple {@code IndexKeyDefinition} instances can be defined.
 * The {@code IndexedCollection} defines based on the passed values which
 * internal structure should be used.
 * 
 * @author pmeisen
 * 
 */
public interface IMultipleKeySupport extends IIndexedCollection {
	// nothing special to be added, but the constructor has to support multiple
	// IndexKeyDefinitions
}
