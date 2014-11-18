package net.meisen.dissertation.impl.parser.query;

import net.meisen.general.genmisc.types.Strings;

/**
 * A definition of a dimension in the form {@code DIM.HIERARCHY.LEVEL}.
 * 
 * @author pmeisen
 * 
 */
public class DimensionSelector {
	private final String dimensionId;
	private final String hierarchyId;
	private final String levelId;

	/**
	 * Default constructor specifying the {@code dimensionId},
	 * {@code hierarchyId}, and {@code levelId}.
	 * 
	 * @param dimensionId
	 *            the identifier of the dimension
	 * @param hierarchyId
	 *            the identifier of the hierarchy
	 * @param levelId
	 *            the identifier of the level
	 */
	public DimensionSelector(final String dimensionId,
			final String hierarchyId, final String levelId) {
		this.dimensionId = dimensionId;
		this.hierarchyId = hierarchyId;
		this.levelId = levelId;
	}

	/**
	 * Gets the dimension's identifier defined.
	 * 
	 * @return the dimension's identifier defined.
	 */
	public String getDimensionId() {
		return dimensionId;
	}

	/**
	 * Gets the hierarchy's identifier defined.
	 * 
	 * @return the hierarchy's identifier defined.
	 */
	public String getHierarchyId() {
		return hierarchyId;
	}

	/**
	 * Gets the level's identifier defined.
	 * 
	 * @return the level's identifier defined.
	 */
	public String getLevelId() {
		return levelId;
	}

	@Override
	public String toString() {
		return Strings.concate(".", getDimensionId(), getHierarchyId(),
				getLevelId());
	}
}
