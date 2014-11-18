package net.meisen.dissertation.impl.parser.query;

import net.meisen.general.genmisc.types.Strings;

public class DimensionSelector {
	private final String dimensionId;
	private final String hierarchyId;
	private final String levelId;

	public DimensionSelector(final String dimensionId,
			final String hierarchyId, final String levelId) {
		this.dimensionId = dimensionId;
		this.hierarchyId = hierarchyId;
		this.levelId = levelId;
	}

	public String getDimensionId() {
		return dimensionId;
	}

	public String getHierarchyId() {
		return hierarchyId;
	}

	public String getLevelId() {
		return levelId;
	}

	@Override
	public String toString() {
		return Strings.concate(".", getDimensionId(), getHierarchyId(),
				getLevelId());
	}
}
