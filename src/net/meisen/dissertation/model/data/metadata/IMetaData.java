package net.meisen.dissertation.model.data.metadata;

import java.util.Collection;

import net.meisen.dissertation.model.descriptors.DescriptorModel;

/**
 * {@code MetaData} is used to describe data by a value. A {@code MetaData}
 * instance is associated to a specific {@code DescriptorModel}. It provides
 * different values (used to create the descriptors of the model).
 * 
 * @see DescriptorModel
 * 
 * @author pmeisen
 * 
 */
public interface IMetaData {

	/**
	 * Gets the identifier of the {@code DescriptorModel} the {@code MetaData}
	 * belongs to.
	 * 
	 * @return the identifier of the {@code DescriptorModel} the
	 *         {@code MetaData} belongs to
	 */
	public String getDescriptorModelId();

	/**
	 * Gets the values of the descriptors defined by {@code this}.
	 * 
	 * @return the values of the descriptors defined by {@code this}
	 */
	public Collection<Object> getValues();

	/**
	 * Gets the amount of values.
	 * 
	 * @return the amount of values
	 */
	public int size();
}
