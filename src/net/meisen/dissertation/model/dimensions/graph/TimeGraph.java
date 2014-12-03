package net.meisen.dissertation.model.dimensions.graph;

import java.util.Collections;
import java.util.Date;
import java.util.HashMap;
import java.util.Map;
import java.util.Set;

import net.meisen.dissertation.exceptions.TimeDimensionException;
import net.meisen.dissertation.impl.time.granularity.TimeGranularityFactory;
import net.meisen.dissertation.model.data.IntervalModel;
import net.meisen.dissertation.model.dimensions.IDimension;
import net.meisen.dissertation.model.dimensions.TimeDimension;
import net.meisen.dissertation.model.dimensions.TimeHierarchy;
import net.meisen.dissertation.model.dimensions.TimeLevel;
import net.meisen.dissertation.model.dimensions.TimeLevelMember;
import net.meisen.dissertation.model.dimensions.templates.ITimeLevelTemplate;
import net.meisen.dissertation.model.dimensions.templates.TimeLevelTemplateManager;
import net.meisen.dissertation.model.time.mapper.BaseMapper;
import net.meisen.general.genmisc.exceptions.ForwardedRuntimeException;

/**
 * The implementation of a graph used for a dimension of time.
 * 
 * @author pmeisen
 * 
 */
public class TimeGraph implements IDimensionGraph {
	private final TimeLevelTemplateManager timeLevelTemplateManager;
	private final IntervalModel intervalModel;
	private final TimeGranularityFactory granularityFactory;
	private final Map<String, TimeGraphLevelIndex> idx;

	private TimeDimension dimension = null;

	public TimeGraph(final IntervalModel intervalModel,
			final TimeGranularityFactory granularityFactory,
			final TimeLevelTemplateManager timeLevelTemplateManager) {
		this.timeLevelTemplateManager = timeLevelTemplateManager;
		this.intervalModel = intervalModel;
		this.granularityFactory = granularityFactory;
		this.idx = new HashMap<String, TimeGraphLevelIndex>();
	}

	@Override
	public void create(final IDimension dimension)
			throws ForwardedRuntimeException {

		// make sure the dimension is correct
		if (dimension instanceof TimeDimension == false) {
			throw new ForwardedRuntimeException(TimeDimensionException.class,
					1009, dimension == null ? null : dimension.getClass()
							.getSimpleName());
		}

		this.dimension = (TimeDimension) dimension;
		this.idx.clear();

		for (final TimeHierarchy h : this.dimension.getHierarchies()) {
			final TimeGraphLevelIndex idx = new TimeGraphLevelIndex(
					intervalModel);
			if (this.idx.containsKey(h.getId())) {
				// TODO
				throw new IllegalArgumentException();
			}
			this.idx.put(h.getId(), idx);

			for (final TimeLevel l : h.getLevels()) {
				final ITimeLevelTemplate template = timeLevelTemplateManager
						.getTemplate(granularityFactory, l.getTemplateId());
				if (template == null) {
					// TODO
					throw new IllegalArgumentException();
				}

				idx.add(l, template, h.getTimeZone());
			}
		}
	}

	public boolean isLazy(final String hierarchyId, final String levelId) {
		final TimeGraphLevelIndex hierarchyIdx = this.idx.get(hierarchyId);
		if (hierarchyIdx == null) {
			return false;
		} else {
			return hierarchyIdx.isLazy(levelId);
		}
	}

	public Set<TimeLevelMember> getMembers(final String hierarchyId,
			final String levelId, final Date start, final Date end) {
		final BaseMapper<?> mapper = this.intervalModel.getTimelineMapper();

		final long s = mapper.mapToLong(start);
		final long e = mapper.mapToLong(end);

		return getMembers(hierarchyId, levelId, s, e);
	}

	public Set<TimeLevelMember> getMembers(final String hierarchyId,
			final String levelId, final long start, final long end) {
		final TimeGraphLevelIndex hierarchyIdx = this.idx.get(hierarchyId);
		if (hierarchyIdx == null) {
			return Collections.<TimeLevelMember> emptySet();
		} else {
			return hierarchyIdx.getMembers(levelId, start, end);
		}
	}

	@Override
	public TimeDimension getDimension() {
		return dimension;
	}
}
