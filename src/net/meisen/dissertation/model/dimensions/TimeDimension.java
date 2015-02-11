package net.meisen.dissertation.model.dimensions;

import java.util.Collection;
import java.util.HashMap;
import java.util.Map;

import net.meisen.dissertation.exceptions.TimeDimensionException;
import net.meisen.general.genmisc.exceptions.ForwardedRuntimeException;

/**
 * A definition of a time-dimension.
 * 
 * @author pmeisen
 * 
 */
public class TimeDimension implements IDimension {
	/**
	 * The identifier used for any root-level.
	 */
	public final static String ROOT_LEVEL_ID = "*";
	/**
	 * The identifier used for the root-level's template.
	 */
	public final static String ROOT_LEVEL_TEMPLATE_ID = "*";

	private final String id;
	private final String name;
	private final Map<String, TimeHierarchy> hierarchies;

	/**
	 * Constructor used to specify the identifier of the dimension.
	 * 
	 * @param id
	 *            the identifier of the dimension
	 */
	public TimeDimension(final String id) {
		this(id, null);
	}

	/**
	 * Constructor used to specify the identifier and the name of the dimension.
	 * 
	 * @param id
	 *            the identifier of the dimension
	 * @param name
	 *            the name of the dimension (can be {@code null})
	 */
	public TimeDimension(final String id, final String name) {
		this.id = id;
		this.name = name;
		this.hierarchies = new HashMap<String, TimeHierarchy>();
	}

	/**
	 * Creates and adds the specified hierarchy to {@code this}.
	 * 
	 * @param id
	 *            the identifier of the hierarchy to be added
	 * @param name
	 *            the name of the hierarchy to be added
	 * @param timeZone
	 *            the time-zone of the hierarchy to be added
	 * 
	 * @return the created hierarchy
	 * 
	 * @throws ForwardedRuntimeException
	 *             if another hierarchy with the same identifier exists
	 */
	public TimeHierarchy addHierarchy(final String id, final String name,
			final String timeZone) throws ForwardedRuntimeException {
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

	/**
	 * Gets the name of the dimension.
	 * 
	 * @return the name of the dimension
	 */
	public String getName() {
		return name == null ? id : name;
	}

	/**
	 * Gets the hierarchy defined with the specified identifier for {@code this}
	 * .
	 * 
	 * @param hierarchyId
	 *            the identifier of the hierarchy to be retrieved
	 * 
	 * @return the hierarchy or {@code null} if none with the specified
	 *         identifier exists
	 */
	public TimeHierarchy getHierarchy(final String hierarchyId) {
		return this.hierarchies.get(hierarchyId);
	}

	@Override
	public boolean hasHierarchy(String hierarchyId) {
		return getHierarchy(hierarchyId) != null;
	}
}