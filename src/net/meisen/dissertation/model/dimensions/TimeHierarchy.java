package net.meisen.dissertation.model.dimensions;

import java.util.Collection;
import java.util.Collections;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.Map;

import net.meisen.dissertation.exceptions.DimensionModelException;
import net.meisen.general.genmisc.exceptions.ForwardedRuntimeException;
import net.meisen.general.genmisc.types.Dates;

/**
 * The definition of a time-hierarchy.
 * 
 * @author pmeisen
 * 
 */
public class TimeHierarchy {
	private final String id;
	private final String name;
	private final String timeZone;
	private final TimeDimension timeDimension;
	private final Map<String, TimeLevel> levels;

	/**
	 * Constructor to create a hierarchy.
	 * 
	 * @param dimension
	 *            the dimension the hierarchy belongs to
	 * @param timeZone
	 *            the time-zone of the hierarchy, if {@code null} the general
	 *            time-zone (i.e. {@link Dates#GENERAL_TIMEZONE}) is used
	 * @param id
	 *            the identifier of the hierarchy
	 * @param name
	 *            the name of the hierarchy
	 */
	public TimeHierarchy(final TimeDimension dimension, final String timeZone,
			final String id, final String name) {
		this.id = id;
		this.name = name;
		this.timeZone = timeZone == null ? Dates.GENERAL_TIMEZONE : timeZone;
		this.timeDimension = dimension;
		this.levels = new LinkedHashMap<String, TimeLevel>();
	}

	/**
	 * Creates and adds the specified level to the hierarchy. The level is
	 * appended to the end of the hierarchy.
	 * 
	 * @param levelId
	 *            the identifier of the level to be added
	 * @param templateId
	 *            the identifier of the template to be used for the level
	 * @param lazy
	 *            {@code true} if the level should be lazy, otherwise
	 *            {@code false}
	 * @return the created level
	 * 
	 * @throws ForwardedRuntimeException
	 *             if another level uses the same identifier
	 */
	public TimeLevel addLevel(final String levelId, final String templateId,
			final boolean lazy) throws ForwardedRuntimeException {
		final TimeLevel level = new TimeLevel(this, levelId, templateId, lazy);
		if (levels.put(levelId, level) != null) {
			throw new ForwardedRuntimeException(DimensionModelException.class,
					1011, levelId, getId());
		}

		return level;
	}

	/**
	 * Gets the root-level of {@code this}.
	 * 
	 * @return the root-level of {@code this}
	 */
	public TimeLevel getRootLevel() {
		return levels.values().iterator().next();
	}

	/**
	 * Modifies a level by setting the level's name.
	 * 
	 * @param levelId
	 *            the identifier of the level
	 * @param name
	 *            the name to be set for the level
	 */
	public void modifyLevel(final String levelId, final String name) {
		final TimeLevel level = levels.get(levelId);
		level.setName(name);
	}

	/**
	 * Gets the dimension the hierarchy belongs to.
	 * 
	 * @return the dimension the hierarchy belongs to
	 */
	public TimeDimension getTimeDimension() {
		return timeDimension;
	}

	/**
	 * Gets the time-zone of {@code this}.
	 * 
	 * @return the time-zone of {@code this}
	 */
	public String getTimeZone() {
		return timeZone;
	}

	/**
	 * Gets the identifier of {@code this}.
	 * 
	 * @return the identifier of {@code this}
	 */
	public String getId() {
		return id;
	}

	/**
	 * Gets the name of {@code this}.
	 * 
	 * @return the name of {@code this}
	 */
	public String getName() {
		return name == null ? id : name;
	}

	/**
	 * Gets an iterator to iterate over the levels specified for the hierarchy.
	 * 
	 * @return iterator for the levels
	 */
	public Iterator<TimeLevel> it() {
		return levels.values().iterator();
	}

	/**
	 * Gets the amount of levels specified for the hierarchy.
	 * 
	 * @return the amount of levels specified for the hierarchy
	 */
	public int sizeOfLevels() {
		return levels.size();
	}

	/**
	 * Gets the level defined at position {@code pos}. The {@code 0}-position is
	 * the root-level, whereby the {@code 1}-position is the one after that one
	 * and so on.
	 * 
	 * @param pos
	 *            the position of the level to get
	 * 
	 * @return the level at the specified position
	 */
	public TimeLevel getLevel(final int pos) {
		return net.meisen.general.genmisc.collections.Collections.get(pos,
				levels.values());
	}

	/**
	 * Get the level with the specified identifier.
	 * 
	 * @param levelId
	 *            the identifier of the level to retrieve
	 * 
	 * @return the level found
	 */
	public TimeLevel getLevel(final String levelId) {
		return levels.get(levelId);
	}

	/**
	 * Gets the levels specified for the hierarchy.
	 * 
	 * @return the levels specified for the hierarchy
	 */
	public Collection<TimeLevel> getLevels() {
		return Collections.unmodifiableCollection(levels.values());
	}
}
