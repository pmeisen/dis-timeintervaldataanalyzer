package net.meisen.dissertation.model.dimensions;

public class TimeLevel {

	private final TimeHierarchy hierarchy;
	private final String id;
	private final String templateId;
	private final boolean lazy;

	private String name;

	public TimeLevel(final TimeHierarchy hierarchy, final String id,
			final String templateId, final boolean lazy) {
		this.hierarchy = hierarchy;
		this.templateId = templateId;
		this.id = id;
		this.lazy = lazy;

		this.name = null;
	}

	public String getName() {
		return name == null ? id : name;
	}

	public void setName(final String name) {
		this.name = name;
	}

	public TimeHierarchy getHierarchy() {
		return hierarchy;
	}

	public String getId() {
		return id;
	}

	@Override
	public String toString() {
		return getId();
	}

	public String getTemplateId() {
		return templateId;
	}

	public boolean isLazy() {
		return lazy;
	}
}
