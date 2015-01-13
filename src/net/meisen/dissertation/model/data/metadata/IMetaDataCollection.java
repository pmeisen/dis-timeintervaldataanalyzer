package net.meisen.dissertation.model.data.metadata;

import java.util.Collection;

import net.meisen.general.genmisc.exceptions.ForwardedRuntimeException;

/**
 * A meta-data collection is used to store information about the meta-data.
 * Generally there can be two different kinds of meta-data:
 * <ul>
 * <li>defined meta-data</li>
 * <li>defined and indexed meta-data</li>
 * </ul>
 * . The first is created e.g. when retrieving the data from a database or just
 * defining the values via XML. The latter is available as soon as a descriptor
 * is created for the meta-data, i.e. a system identifier is available and
 * known. The collection has to keep track of the different types, whereby a
 * once indexed meta-data cannot be changed to not-indexed. A not-index
 * meta-data can be changed to a indexed one. Finally, the collection has to
 * ensure that there are no duplicates collected, i.e. within each
 * {@code DescriptorModel} there can only be one {@code Descriptor} with a
 * specific value.
 * 
 * @author pmeisen
 * 
 */
public interface IMetaDataCollection extends Iterable<IMetaData> {

	/**
	 * Adds all the {@code MetaData} to {@code this}.
	 * 
	 * @param metaData
	 *            the collection of {@code MetaData} to be added
	 * 
	 * @throws ForwardedRuntimeException
	 *             if the meta data to be added is invalid, e.g. an identifier
	 *             is changed, or the value of an identifier is modified
	 */
	public void addMetaData(final Collection<IMetaData> metaData)
			throws ForwardedRuntimeException;

	/**
	 * Adds the {@code MetaData} to {@code this}.
	 * 
	 * @param metaData
	 *            the {@code MetaData} to be added
	 * 
	 * @throws ForwardedRuntimeException
	 *             if the meta data to be added is invalid, e.g. an identifier
	 *             is changed, or the value of an identifier is modified
	 */
	public void addMetaData(final IMetaData metaData)
			throws ForwardedRuntimeException;

	/**
	 * Sets the {@code MetaData} to be collected.
	 * 
	 * @param metaData
	 *            the {@code MetaData} to be collected
	 * 
	 * @throws ForwardedRuntimeException
	 *             if the meta data to be added is invalid, e.g. an identifier
	 *             is changed, or the value of an identifier is modified
	 */
	public void setMetaData(final Collection<IMetaData> metaData)
			throws ForwardedRuntimeException;

	/**
	 * Adds the {@code MetaData} of the passed {@code collection} to
	 * {@code this}.
	 * 
	 * @param collection
	 *            the {@code MetaDataCollection} to add the {@code MetaData}
	 *            from
	 */
	public void add(final IMetaDataCollection collection);

	/**
	 * Removes all the values from the collection.
	 */
	public void clear();

	/**
	 * Get the amount of {@code DescriptorModel} associations.
	 * 
	 * @return the amount of {@code DescriptorModel} associations
	 */
	public int size();

	/**
	 * Get the amount of {@code MetaData} instances associated to the specified
	 * {@code DescriptorModel}.
	 * 
	 * @param descriptorModelId
	 *            the identifier of the {@code DescriptorModel} to get the
	 *            amount of {@code MetaData} instances from
	 * 
	 * @return the amount of {@code MetaData} instances associated to the
	 *         specified {@code DescriptorModel}
	 */
	public int size(final String descriptorModelId);

	/**
	 * Determines the amount of values available for a specified
	 * {@code descriptorModelId}.
	 * 
	 * @param descriptorModelId
	 *            the identifier of the {@code DescriptorModel} to determine the
	 *            size for
	 * 
	 * @return the amount of values available for a specified
	 *         {@code descriptorModelId}
	 */
	public int sizeOfValues(final String descriptorModelId);

	/**
	 * Gets all the defined meta-data for the specified
	 * {@code descriptorModelId}. The collection does not contain any
	 * duplicates, considering the values of the model.
	 * 
	 * @param descriptorModelId
	 *            the identifier of the {@code DescriptorModel} to get the
	 *            meta-data for
	 * 
	 * @return a collection of all the meta-data defined for the specified
	 *         {@code descriptorModelId}
	 */

	public Collection<IMetaData> get(final String descriptorModelId);
}
