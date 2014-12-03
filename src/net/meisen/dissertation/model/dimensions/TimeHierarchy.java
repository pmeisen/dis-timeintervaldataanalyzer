package net.meisen.dissertation.model.dimensions;

import java.util.Collection;
import java.util.Collections;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.TimeZone;

import net.meisen.general.genmisc.types.Dates;

public class TimeHierarchy {

	private final String id;
	private final String name;
	private final String timeZone;
	private final TimeDimension timeDimension;
	private final Map<String, TimeLevel> levels;

	public TimeHierarchy(final TimeDimension timeDimension,
			final String timeZone, final String id, final String name) {
		this.id = id;
		this.name = name;
		this.timeZone = timeZone == null ? Dates.GENERAL_TIMEZONE : timeZone;
		this.timeDimension = timeDimension;
		this.levels = new LinkedHashMap<String, TimeLevel>();

	}

	public TimeLevel addLevel(final String levelId, final String templateId,
			final boolean lazy) {
		final TimeLevel level = new TimeLevel(this, levelId, templateId, lazy);
		levels.put(levelId, level);

		return level;
	}

	public TimeLevel getRootLevel() {
		return levels.values().iterator().next();
	}

	public void modifyLevel(final String levelId, final String name) {
		final TimeLevel level = levels.get(levelId);
		level.setName(name);
	}

	public TimeDimension getTimeDimension() {
		return timeDimension;
	}

	public String getTimeZone() {
		return timeZone;
	}

	public String getId() {
		return id;
	}

	public String getName() {
		return name == null ? id : name;
	}

	public Iterator<TimeLevel> it() {
		return levels.values().iterator();
	}

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

	public Collection<TimeLevel> getLevels() {
		return Collections.unmodifiableCollection(levels.values());
	}
}
