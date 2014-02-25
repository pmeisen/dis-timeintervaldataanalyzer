package net.meisen.dissertation.model.persistence;

import java.io.File;

/**
 * A {@code Location} which can be represented by a {@code File}.
 * 
 * @author pmeisen
 * 
 * @see File
 * @see ILocation
 */
public interface IFileLocation extends ILocation {

	/**
	 * Gets the {@code File} represented by the location.
	 * 
	 * @return the {@code File} represented by the location
	 */
	public File getFile();
}
