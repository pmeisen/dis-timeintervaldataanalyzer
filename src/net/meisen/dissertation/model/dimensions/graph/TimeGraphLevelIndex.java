package net.meisen.dissertation.model.dimensions.graph;

import java.util.Collections;
import java.util.HashMap;
import java.util.Iterator;
import java.util.LinkedHashSet;
import java.util.Map;
import java.util.Set;

import net.meisen.dissertation.model.data.IntervalModel;
import net.meisen.dissertation.model.dimensions.TimeLevel;
import net.meisen.dissertation.model.dimensions.TimeLevelMember;
import net.meisen.dissertation.model.dimensions.templates.ITimeLevelTemplate;

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

	public TimeGraphLevelIndex(final IntervalModel intervalModel) {
		this(intervalModel, HashMap.class);
	}

	@SuppressWarnings("unchecked")
	public TimeGraphLevelIndex(final IntervalModel intervalModel,
			final Class<? extends Map> type) {
		try {
			map = type.newInstance();
			lazyMap = type.newInstance();
		} catch (final Exception e) {
			throw new IllegalStateException("Invalid map-type.");
		}

		this.intervalModel = intervalModel;
	}

	public void add(final TimeLevel l, final ITimeLevelTemplate template,
			final String timezone) {

		if (l.isLazy()) {
			if (lazyMap.containsKey(l.getId())) {
				// TODO make it nice
				throw new IllegalArgumentException();
			}
			lazyMap.put(l.getId(), new LazyTemplate(template, timezone));
		} else {

			if (map.containsKey(l.getId())) {
				// TODO make it nice
				throw new IllegalArgumentException();
			}

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

	public boolean isLazy(final String levelId) {
		return lazyMap.containsKey(levelId);
	}

	public Set<TimeLevelMember> getMembers(final String levelId,
			final long start, final long end) {
		final TimeGraphMemberIndex levelIdx = map.get(levelId);
		if (levelIdx == null) {
			return lazyLoadMembers(levelId, start, end);
		} else {
			return levelIdx.getMembers(start, end);
		}
	}

	public Set<TimeLevelMember> lazyLoadMembers(final String levelId,
			final long start, final long end) {

		final LazyTemplate lazyTemplate = lazyMap.get(levelId);
		if (lazyTemplate == null) {
			return Collections.<TimeLevelMember> emptySet();
		} else {
			final Iterator<TimeLevelMember> it = lazyTemplate.template.it(
					intervalModel, start, end, lazyTemplate.getTimezone());
			final Set<TimeLevelMember> members = new LinkedHashSet<TimeLevelMember>();
			while (it.hasNext()) {
				members.add(it.next());
			}

			return members;
		}
	}

	public boolean isValidLevel(final String levelId) {
		return lazyMap.containsKey(levelId);
	}
}
