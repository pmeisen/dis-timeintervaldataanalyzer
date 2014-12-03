package net.meisen.dissertation.model.dimensions;

import java.util.Collection;
import java.util.HashMap;
import java.util.Map;

import net.meisen.dissertation.exceptions.TimeDimensionException;
import net.meisen.general.genmisc.exceptions.ForwardedRuntimeException;

public class TimeDimension implements IDimension {
	public final static String ROOT_LEVEL_ID = "*";
	public final static String ROOT_LEVEL_TEMPLATE_ID = "*";

	private final String id;
	private final String name;
	private final Map<String, TimeHierarchy> hierarchies;

	public TimeDimension(final String id) {
		this(id, null);
	}

	public TimeDimension(final String id, final String name) {
		this.id = id;
		this.name = name;
		this.hierarchies = new HashMap<String, TimeHierarchy>();
	}

	public TimeHierarchy addHierarchy(final String id, final String name,
			final String timeZone) {
		if (hierarchies.containsKey(id)) {
			throw new ForwardedRuntimeException(TimeDimensionException.class,
					1001, id);
		}

		// create the hierarchy with the root
		final TimeHierarchy hierarchy = new TimeHierarchy(this, timeZone, id,
				name);
		hierarchy.addLevel(ROOT_LEVEL_ID, ROOT_LEVEL_TEMPLATE_ID, false);
		this.hierarchies.put(id, hierarchy);

		return hierarchy;
	}

	/**
	 * Get all the defined hierarchies.
	 * 
	 * @return the defined hierarchies
	 */
	public Collection<TimeHierarchy> getHierarchies() {
		return this.hierarchies.values();
	}

	@Override
	public String getId() {
		return id;
	}

	public String getName() {
		return name == null ? id : name;
	}

	public TimeHierarchy getHierarchy(final String hierarchyId) {
		return this.hierarchies.get(hierarchyId);
	}
}