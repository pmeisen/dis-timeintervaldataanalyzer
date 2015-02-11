package net.meisen.dissertation.model.dimensions;

/**
 * Interface of a dimension definition.
 * 
 * @author pmeisen
 * 
 */
public interface IDimension {

	/**
	 * Gets the identifier of the dimension.
	 * 
	 * @return the identifier of the dimension
	 */
	public String getId();

	/**
	 * Validates if the dimension has the specified hierarchy.
	 * 
	 * @param hierarchyId
	 *            the identifier of the hierarchy
	 *            
	 * @return {@code true} if such an hierarchy exists, otherwise {@code false}
	 */
	public boolean hasHierarchy(final String hierarchyId);
}
