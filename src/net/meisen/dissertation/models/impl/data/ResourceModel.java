package net.meisen.dissertation.models.impl.data;

/**
 * A {@code ResourceModel} which generally defines {@code Resource} instances.
 * 
 * @author pmeisen
 * 
 */
public class ResourceModel {
	private String id;
	private String name;

	/**
	 * Constructor which defines the {@code id} of the model. The {@code name}
	 * is equal to the passed id.
	 * 
	 * @param id
	 *            the id of the model
	 */
	public ResourceModel(final String id) {
		this(id, id);
	}

	/**
	 * Constructor to define the {@code id} and the {@code name} of the model.
	 * 
	 * @param id
	 *            the id of the model
	 * @param name
	 *            the name of the model
	 */
	public ResourceModel(final String id, final String name) {
		this.id = id;
		this.name = name;
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
