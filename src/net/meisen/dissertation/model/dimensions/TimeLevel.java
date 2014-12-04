package net.meisen.dissertation.model.dimensions;

/**
 * A definition of a level of the time-dimension.
 * 
 * @author pmeisen
 * 
 */
public class TimeLevel {

	private final TimeHierarchy hierarchy;
	private final String id;
	private final String templateId;
	private final boolean lazy;

	private String name;

	/**
	 * Creates an instance of a definition of a time-level.
	 * 
	 * @param hierarchy
	 *            the hierarchy the level belongs to
	 * @param id
	 *            the identifier of the level defined by {@code this}
	 * @param templateId
	 *            the identifier of the template for {@code this}
	 * @param lazy
	 *            {@code true} if the level is loaded lazy, i.e. the members are
	 *            not kept in memory, instead the system creates them on demand,
	 *            otherwise {@code false}
	 */
	public TimeLevel(final TimeHierarchy hierarchy, final String id,
			final String templateId, final boolean lazy) {
		this.hierarchy = hierarchy;
		this.templateId = templateId;
		this.id = id;
		this.lazy = lazy;

		this.name = null;
	}

	/**
	 * Gets the name of the level.
	 * 
	 * @return the name of the level
	 */
	public String getName() {
		return name == null ? id : name;
	}

	/**
	 * Sets the name of the level.
	 * 
	 * @param name
	 *            the name of the level
	 */
	public void setName(final String name) {
		this.name = name;
	}

	/**
	 * Gets the hierarchy the level belongs to.
	 * 
	 * @return the hierarchy the level belongs to
	 */
	public TimeHierarchy getHierarchy() {
		return hierarchy;
	}

	/**
	 * Gets the identifier of {@code this}.
	 * 
	 * @return the identifier of {@code this}
	 */
	public String getId() {
		return id;
	}

	@Override
	public String toString() {
		return getId();
	}

	/**
	 * Gets the identifier of the template of the level.
	 * 
	 * @return the identifier of the template of the level
	 */
	public String getTemplateId() {
		return templateId;
	}

	/**
	 * Checks if the level is defined to be lazy, i.e. the members are not kept
	 * in memory, instead the system creates them on demand.
	 * 
	 * @return {@code true} if the level is lazy, otherwise {@code false}
	 */
	public boolean isLazy() {
		return lazy;
	}
}
