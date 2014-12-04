package net.meisen.dissertation.model.dimensions.graph;

import java.util.Collections;
import java.util.Date;
import java.util.HashMap;
import java.util.Map;
import java.util.Set;

import net.meisen.dissertation.exceptions.DimensionModelException;
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

	/**
	 * Constructor creating a {@code TimeGraph} for the specified model using
	 * the {@code granularityFactory} and the defined
	 * {@code timeLevelTemplateManager}.
	 * 
	 * @param intervalModel
	 *            the {@code IntervalModel} to be used for the graph
	 * @param granularityFactory
	 *            the factory to be used to create granularities
	 * @param timeLevelTemplateManager
	 *            the manager used to retrieve templates
	 */
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
				throw new ForwardedRuntimeException(
						DimensionModelException.class, 1009, h.getId());
			}
			this.idx.put(h.getId(), idx);

			for (final TimeLevel l : h.getLevels()) {
				final ITimeLevelTemplate template = timeLevelTemplateManager
						.getTemplate(granularityFactory, l.getTemplateId());
				if (template == null) {
					throw new ForwardedRuntimeException(
							DimensionModelException.class, 1010,
							l.getTemplateId(), l.getId(), h.getId());
				}

				idx.add(l, template, h.getTimeZone());
			}
		}
	}

	/**
	 * Checks if the specified level is defined to be lazy, i.e. the level
	 * doesn't have any index instead the members are created when needed.
	 * 
	 * @param hierarchyId
	 *            the identifier of the hierarchy
	 * @param levelId
	 *            the identifier of the level
	 * 
	 * @return {@code true} if the level is lazy, otherwise {@code false}
	 */
	public boolean isLazy(final String hierarchyId, final String levelId) {
		final TimeGraphLevelIndex hierarchyIdx = this.idx.get(hierarchyId);
		if (hierarchyIdx == null) {
			return false;
		} else {
			return hierarchyIdx.isLazy(levelId);
		}
	}

	/**
	 * Gets the members of the specified level, within the specified range.
	 * 
	 * @param hierarchyId
	 *            the identifier of the hierarchy
	 * @param levelId
	 *            the identifier of the level to get the members for
	 * @param start
	 *            the temporal start value of the members of the level to be
	 *            retrieved
	 * @param end
	 *            the temporal end value of the members of the level to be
	 *            retrieved
	 * 
	 * @return the members
	 */
	public Set<TimeLevelMember> getMembers(final String hierarchyId,
			final String levelId, final Date start, final Date end) {
		final BaseMapper<?> mapper = this.intervalModel.getTimelineMapper();

		final long s = mapper.mapToLong(start);
		final long e = mapper.mapToLong(end);

		return getMembers(hierarchyId, levelId, s, e);
	}

	/**
	 * Gets the members of the specified level, within the specified range.
	 * 
	 * @param hierarchyId
	 *            the identifier of the hierarchy
	 * @param levelId
	 *            the identifier of the level to get the members for
	 * @param start
	 *            the temporal start value of the members of the level to be
	 *            retrieved
	 * @param end
	 *            the temporal end value of the members of the level to be
	 *            retrieved
	 * 
	 * @return the members
	 */
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

	/**
	 * Checks if the graph has a hierarchy with the specified identifier.
	 * 
	 * @param hierarchyId
	 *            the hierarchy to be checked
	 * 
	 * @return {@code true} if such a hierarchy exists, otherwise {@code false}
	 */
	public boolean isValidSelection(final String hierarchyId) {
		return isValidSelection(hierarchyId, null);
	}

	/**
	 * Checks if the graph has a level with the specified identifier within the
	 * specified hierarchy.
	 * 
	 * @param hierarchyId
	 *            the hierarchy's identifier
	 * @param levelId
	 *            the identifier of the level to be checked
	 * 
	 * @return {@code true} if such a level exists, otherwise {@code false}
	 */
	public boolean isValidSelection(final String hierarchyId,
			final String levelId) {
		final TimeGraphLevelIndex hierarchy = this.idx.get(hierarchyId);
		if (hierarchy == null) {
			return false;
		} else if (levelId == null) {
			return true;
		} else {
			return hierarchy.isValidLevel(levelId);
		}
	}
}
