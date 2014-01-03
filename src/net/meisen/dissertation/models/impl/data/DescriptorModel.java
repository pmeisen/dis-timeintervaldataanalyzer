package net.meisen.dissertation.models.impl.data;

/**
 * A {@code DescriptorModel} which generally defines {@code Descriptor}
 * instances.
 * 
 * @author pmeisen
 * 
 */
public class DescriptorModel {
	private String id;
	private String name;
	private Class<?> dataType;

	/**
	 * Constructor to define the {@code id} and the {@code dataType} of the
	 * model's data.
	 * 
	 * @param id
	 *            the id of the model
	 * @param dataType
	 *            the type of the data associated to the model
	 */
	public DescriptorModel(final String id, final Class<?> dataType) {
		this(id, id, dataType);
	}

	/**
	 * Constructor to define the {@code id}, {@code name} and the
	 * {@code dataType} of the model's data.
	 * 
	 * @param id
	 *            the id of the model
	 * @param name
	 *            the name of the model
	 * @param dataType
	 *            the type of the data associated to the model
	 */
	public DescriptorModel(final String id, final String name,
			final Class<?> dataType) {
		this.id = id;
		this.name = name;
		this.dataType = dataType;
	}

	/**
	 * Gets the type of the data of this descriptor.
	 * 
	 * @return the type of the data of this descriptor
	 */
	public Class<?> getDataType() {
		return dataType;
	}

	/**
	 * Gets the name of the model.
	 * 
	 * @return the name of the model
	 */
	public String getName() {
		return name;
	}

	/**
	 * Gets the id of the model.
	 * 
	 * @return the id of the model
	 */
	public String getId() {
		return id;
	}

	@Override
	public boolean equals(final Object o) {
		boolean res = false;

		if (o == this) {
			res = true;
		} else if (o instanceof DescriptorModel) {
			final DescriptorModel cmp = (DescriptorModel) o;

			// check the attributes
			if (id.equals(cmp.getId())) {
				return true;
			}
		}

		return res;
	}
}
