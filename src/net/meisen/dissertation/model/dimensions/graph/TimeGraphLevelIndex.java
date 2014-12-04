package net.meisen.dissertation.model.dimensions.graph;

import java.util.Collections;
import java.util.HashMap;
import java.util.Iterator;
import java.util.LinkedHashSet;
import java.util.Map;
import java.util.Set;

import net.meisen.dissertation.exceptions.DimensionModelException;
import net.meisen.dissertation.model.data.IntervalModel;
import net.meisen.dissertation.model.dimensions.TimeLevel;
import net.meisen.dissertation.model.dimensions.TimeLevelMember;
import net.meisen.dissertation.model.dimensions.templates.ITimeLevelTemplate;
import net.meisen.general.genmisc.exceptions.ForwardedRuntimeException;

/**
 * Implementation used to index the templates of a level.
 * 
 * @author pmeisen
 * 
 * @see TimeLevel
 * 
 */
public class TimeGraphLevelIndex {
	private final IntervalModel intervalModel;

	private final Map<String, LazyTemplate> lazyMap;
	private final Map<String, TimeGraphMemberIndex> map;

	private static class LazyTemplate {
		private final String timezone;
		private final ITimeLevelTemplate template;

		public LazyTemplate(final ITimeLevelTemplate template,
				final String timezone) {
			this.template = template;
			this.timezone = timezone;
		}

		public String getTimezone() {
			return timezone;
		}

		public ITimeLevelTemplate getTemplate() {
			return template;
		}
	}

	/**
	 * Creates an index for the specified {@code intervalModel} using a
	 * {@code HashMap} internally.
	 * 
	 * @param intervalModel
	 *            the model to create the index for
	 */
	public TimeGraphLevelIndex(final IntervalModel intervalModel) {
		this(intervalModel, HashMap.class);
	}

	/**
	 * Creates an index for the specified {@code intervalModel} using the
	 * {@code type} internally.
	 * 
	 * @param intervalModel
	 *            the model to create the index for
	 * @param type
	 *            the type of the map to be used internally
	 */
	@SuppressWarnings("unchecked")
	public TimeGraphLevelIndex(final IntervalModel intervalModel,
			@SuppressWarnings("rawtypes") final Class<? extends Map> type) {
		try {
			map = type.newInstance();
			lazyMap = type.newInstance();
		} catch (final Exception e) {
			throw new ForwardedRuntimeException(DimensionModelException.class,
					1013, type == null ? null : type.getClass().getSimpleName());
		}

		this.intervalModel = intervalModel;
	}

	/**
	 * Adds the specified level to the index.
	 * 
	 * @param l
	 *            the level to be added
	 * @param template
	 *            the template of the level
	 * @param timezone
	 *            the time-zone of the level
	 */
	public void add(final TimeLevel l, final ITimeLevelTemplate template,
			final String timezone) {

		// make sure the identifier is unique
		if (map.containsKey(l.getId()) || lazyMap.containsKey(l.getId())) {
			throw new ForwardedRuntimeException(DimensionModelException.class,
					1009, l.getId(), l.getHierarchy().getId());
		}

		if (l.isLazy()) {
			lazyMap.put(l.getId(), new LazyTemplate(template, timezone));
		} else {

			// create the index for the level
			final TimeGraphMemberIndex levelIdx = new TimeGraphMemberIndex();
			map.put(l.getId(), levelIdx);

			// iterate and add the members
			final Iterator<TimeLevelMember> it = template.it(intervalModel,
					timezone);
			while (it.hasNext()) {
				levelIdx.add(it.next());
			}
			levelIdx.initialize();
		}

	}

	/**
	 * Checks if the specified level is defined to be lazy.
	 * 
	 * @param levelId
	 *            the identifier of the level to be checked
	 * 
	 * @return {@code true} if the level is lazy, otherwise {@code false}
	 */
	public boolean isLazy(final String levelId) {
		return lazyMap.containsKey(levelId);
	}

	/**
	 * Gets the members of the specified level, falling into the specified
	 * range.
	 * 
	 * @param levelId
	 *            the identifier of the level
	 * @param start
	 *            the start value of the range
	 * @param end
	 *            the end value of the range
	 * 
	 * @return the members
	 */
	public Set<TimeLevelMember> getMembers(final String levelId,
			final long start, final long end) {
		final TimeGraphMemberIndex levelIdx = map.get(levelId);
		if (levelIdx == null) {
			
			final LazyTemplate lazyTemplate = lazyMap.get(levelId);
			if (lazyTemplate == null) {
				return Collections.<TimeLevelMember> emptySet();
			} else {
				final Iterator<TimeLevelMember> it = lazyTemplate.getTemplate()
						.it(intervalModel, start, end,
								lazyTemplate.getTimezone());
				final Set<TimeLevelMember> members = new LinkedHashSet<TimeLevelMember>();
				while (it.hasNext()) {
					members.add(it.next());
				}

				return members;
			}
		} else {
			return levelIdx.getMembers(start, end);
		}
	}

	/**
	 * Checks if the identifier refers to a valid level.
	 * 
	 * @param levelId
	 *            the identifier to be checked
	 * 
	 * @return {@code true} if a level with the specified identifier exists,
	 *         otherwise {@code false}
	 */
	public boolean isValidLevel(final String levelId) {
		return lazyMap.containsKey(levelId);
	}
}
