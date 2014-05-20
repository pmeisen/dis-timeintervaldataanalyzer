package net.meisen.dissertation.model.data.metadata;

import java.util.Map;

/**
 * Marks a {@code MetaData} instance to have values with an already associated
 * identifier.
 * 
 * @author pmeisen
 * 
 */
public interface IIdentifiedMetaData extends IMetaData {

	/**
	 * Gets the values with their identifiers.
	 * 
	 * @return the values with identifiers
	 */
	public Map<Object, Object> getIdentifiedValues();
}
