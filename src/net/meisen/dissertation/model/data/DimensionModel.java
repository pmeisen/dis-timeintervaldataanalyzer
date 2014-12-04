package net.meisen.dissertation.model.data;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;

import net.meisen.dissertation.config.xslt.DefaultValues;
import net.meisen.dissertation.exceptions.DimensionModelException;
import net.meisen.dissertation.exceptions.TidaDimensionHandlerException;
import net.meisen.dissertation.impl.parser.query.DimensionSelector;
import net.meisen.dissertation.impl.time.granularity.TimeGranularityFactory;
import net.meisen.dissertation.model.descriptors.Descriptor;
import net.meisen.dissertation.model.descriptors.DescriptorModel;
import net.meisen.dissertation.model.dimensions.DescriptorDimension;
import net.meisen.dissertation.model.dimensions.DescriptorMember;
import net.meisen.dissertation.model.dimensions.IDimension;
import net.meisen.dissertation.model.dimensions.TimeDimension;
import net.meisen.dissertation.model.dimensions.TimeLevelMember;
import net.meisen.dissertation.model.dimensions.graph.DescriptorGraph;
import net.meisen.dissertation.model.dimensions.graph.DescriptorGraphLevel;
import net.meisen.dissertation.model.dimensions.graph.IDimensionGraph;
import net.meisen.dissertation.model.dimensions.graph.TimeGraph;
import net.meisen.dissertation.model.dimensions.templates.ITimeLevelTemplate;
import net.meisen.dissertation.model.dimensions.templates.TimeLevelTemplateManager;
import net.meisen.dissertation.model.indexes.BaseIndexFactory;
import net.meisen.dissertation.model.indexes.datarecord.TidaIndex;
import net.meisen.dissertation.model.indexes.datarecord.slices.Bitmap;
import net.meisen.dissertation.model.indexes.datarecord.slices.Slice;
import net.meisen.general.genmisc.exceptions.ForwardedRuntimeException;
import net.meisen.general.genmisc.exceptions.registry.IExceptionRegistry;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;

/**
 * The model specifying the MDM of the model.
 * 
 * @author pmeisen
 * 
 */
public class DimensionModel {

	/**
	 * Filter used to specify which members should be added or not
	 * {@link DimensionModel#getBitmap(DimensionSelector, boolean, IMemberFilter)}
	 * .
	 * 
	 * @author pmeisen
	 * 
	 */
	public static interface IMemberFilter {

		/**
		 * Method used to determine if the {@code member} should be accepted or
		 * not.
		 * 
		 * @param member
		 *            the member to be checked
		 * 
		 * @return {@code true} if it should be accepted, otherwise
		 *         {@code false}
		 */
		public boolean accept(final DescriptorMember member);

		/**
		 * Determines if the algorithm can stop, after the first member is
		 * found.
		 * 
		 * @return {@code true} if the algorithm should stop after the first
		 *         founding, otherwise {@code false}
		 */
		public boolean selectOnlyOne();

	}

	@Autowired
	@Qualifier(DefaultValues.EXCEPTIONREGISTRY_ID)
	private IExceptionRegistry exceptionRegistry;

	@Autowired
	@Qualifier(DefaultValues.METADATAMODEL_ID)
	private MetaDataModel metaDataModel;

	@Autowired
	@Qualifier(DefaultValues.INTERVALMODEL_ID)
	private IntervalModel intervalModel;

	@Autowired
	@Qualifier(DefaultValues.INDEXFACTORY_ID)
	private BaseIndexFactory indexFactory;

	@Autowired
	@Qualifier(DefaultValues.TIMETEMPLATEMANAGER_ID)
	private TimeLevelTemplateManager timeLevelTemplateManager;

	@Autowired
	@Qualifier(DefaultValues.GRANULARITYFACTORY_ID)
	private TimeGranularityFactory granularityFactory;

	private final Map<String, IDimensionGraph> dimensions;

	private boolean initialized;
	private Collection<IDimension> addedDimensions;
	private TidaIndex index;

	/**
	 * Default constructor.
	 */
	public DimensionModel() {
		this.dimensions = new HashMap<String, IDimensionGraph>();

		this.addedDimensions = null;
		this.index = null;
		this.initialized = false;
	}

	/**
	 * Checks if the model is initialized.
	 * 
	 * @return {@code true} if the {@code DimensionModel} is initialized,
	 *         otherwise {@code false}
	 */
	public boolean isInitialized() {
		return initialized;
	}

	/**
	 * Adds the specified dimensions to the {@code DimensionModel}. This can
	 * only be done before or during initialization.
	 * 
	 * @param dimensions
	 *            the dimensions to be added
	 */
	@Autowired(required = false)
	public void addDimensions(final Collection<IDimension> dimensions) {
		if (isInitialized()) {
			exceptionRegistry.throwException(DimensionModelException.class,
					1002);
		}

		// adds the dimensions
		if (addedDimensions == null) {
			addedDimensions = new ArrayList<IDimension>();
		}
		addedDimensions.addAll(dimensions);
	}

	/**
	 * Initializes the {@code DimensionModel}.
	 * 
	 * @param tidaModel
	 *            the {@code TidaModel} the {@code DimensionModel} belongs to
	 */
	public void initialize(final TidaModel tidaModel) {
		if (isInitialized()) {
			exceptionRegistry.throwException(DimensionModelException.class,
					1001);
		}

		// add the dimensions and make sure there aren't any duplicates
		final Map<String, IDimensionGraph> dims = createMap(addedDimensions);
		for (final Entry<String, IDimensionGraph> e : dims.entrySet()) {
			if (this.dimensions.put(e.getKey(), e.getValue()) != null) {
				exceptionRegistry.throwException(DimensionModelException.class,
						1000, e.getKey());
			}
		}

		// get the index
		this.index = tidaModel.getIndex();
		this.addedDimensions = null;
		this.initialized = true;
	}

	/**
	 * Gets the {@code DimensionGraph} for the specified {@code dimensionId}.
	 * 
	 * @param dimensionId
	 *            the identifier to get the graph for
	 * 
	 * @return the {@code IDimensionGraph} or {@code null} if not graph is
	 *         defined for the specified id
	 */
	public IDimensionGraph getDimension(final String dimensionId) {
		return this.dimensions.get(dimensionId);
	}

	/**
	 * Get the defined dimensions.
	 * 
	 * @return the defined dimensions
	 */
	public Collection<IDimensionGraph> getDimensions() {
		return Collections.unmodifiableCollection(this.dimensions.values());
	}

	/**
	 * Creates a {@code Bitmap} containing all the records defined by the
	 * specified {@code DimensionSelector}.
	 * 
	 * @param dimSelector
	 *            the {@code DimensionSelector} to get the bitmap for
	 * @param createBitmapWhenEmpty
	 *            {@code true} if an empty bitmap should be returned, if the
	 *            dimension selects nothing, {@code false} if {@code null}
	 *            should be returned
	 * 
	 * @return the created {@code Bitmap}, can only return {@code null} if the
	 *         {@code createBitmapWhenEmpty} flag is set to {@code false}
	 */
	public Bitmap getBitmap(final DimensionSelector dimSelector,
			final boolean createBitmapWhenEmpty) {
		return getBitmap(dimSelector, createBitmapWhenEmpty, null);
	}

	/**
	 * Creates a {@code Bitmap} containing all the records defined by the
	 * specified {@code DimensionSelector}.
	 * 
	 * @param dimSelector
	 *            the {@code DimensionSelector} to get the bitmap for
	 * @param createBitmapWhenEmpty
	 *            {@code true} if an empty bitmap should be returned, if the
	 *            dimension selects nothing, {@code false} if {@code null}
	 *            should be returned
	 * @param filter
	 *            a {@code MemberFilter} used to filter the members of the
	 *            selected level
	 * 
	 * @return the created {@code Bitmap}, can only return {@code null} if the
	 *         {@code createBitmapWhenEmpty} flag is set to {@code false}
	 */
	public Bitmap getBitmap(final DimensionSelector dimSelector,
			final boolean createBitmapWhenEmpty, final IMemberFilter filter) {
		final String dimId = dimSelector.getDimensionId();

		final IDimensionGraph d = getDimension(dimId);
		if (d instanceof DescriptorGraph == false) {
			exceptionRegistry.throwException(DimensionModelException.class,
					1003, dimId);
		}
		final DescriptorGraph dim = (DescriptorGraph) d;

		// get the DescrptorModel used
		final String modelId = dim.getDimension().getDescriptorModelId();
		final DescriptorModel<?> descModel = metaDataModel
				.getDescriptorModel(modelId);
		if (descModel == null) {
			exceptionRegistry.throwException(DimensionModelException.class,
					1004, dimId, modelId);
		}

		// get the level of the defined hierarchy
		final String hierarchyId = dimSelector.getHierarchyId();
		final String levelId = dimSelector.getLevelId();
		final DescriptorGraphLevel descriptorGraphLevel = dim.getLevel(
				hierarchyId, levelId);
		if (descriptorGraphLevel == null) {
			exceptionRegistry.throwException(DimensionModelException.class,
					1005, dimId, hierarchyId, levelId);
		}

		// get all the members selected
		final Set<DescriptorMember> members = descriptorGraphLevel
				.getMembers(hierarchyId);
		final Set<DescriptorMember> selectedMembers = new HashSet<DescriptorMember>();
		for (final DescriptorMember member : members) {

			if (filter == null || filter.accept(member)) {
				final Set<DescriptorMember> leafMembers = descriptorGraphLevel
						.getLeafMembers(hierarchyId, member.getId());
				selectedMembers.addAll(leafMembers);

				if (filter != null && filter.selectOnlyOne()) {
					break;
				}
			}
		}

		// get the bitmaps of the leaf-members
		final List<Bitmap> bitmaps = new ArrayList<Bitmap>();
		for (final Descriptor<?, ?, ?> desc : descModel.getAllDescriptors()) {
			final String value = desc.getUniqueString();

			for (final DescriptorMember member : selectedMembers) {
				if (member.containsValue(value)) {

					// get the slice for the model and the descriptor
					final Slice<?> slice = index.getMetaIndexDimensionSlice(
							modelId, desc.getId());

					// add the slice if it's not null
					if (slice != null) {
						bitmaps.add(slice.getBitmap());
					}

					/*
					 * We don't have to check the other members anymore, the
					 * descriptor is added already - why add it a second time.
					 */
					continue;
				}
			}
		}

		// create the result
		if (bitmaps.size() == 0) {
			return createBitmapWhenEmpty ? indexFactory.createBitmap() : null;
		} else if (bitmaps.size() == 1) {
			return bitmaps.get(0);
		} else {
			return Bitmap.or(indexFactory, bitmaps.toArray());
		}
	}

	/**
	 * Get the {@code TimeLevelMember} for the specified {@code dimSelector}.
	 * 
	 * @param dimSelector
	 *            the selector specifying which members to be retrieved
	 * @param start
	 *            the normalized start value of the members to be retrieved
	 *            (inclusive)
	 * @param end
	 *            the normalized end value of the members to be retrieved
	 *            (inclusive)
	 * 
	 * @return the {@code TimeLevelMember} instances
	 */
	public Set<TimeLevelMember> getTimeMembers(
			final DimensionSelector dimSelector, final long start,
			final long end) {
		final String dimId = dimSelector.getDimensionId();
		final String hierarchyId = dimSelector.getHierarchyId();
		final String levelId = dimSelector.getLevelId();
		final IDimensionGraph graph = getDimension(dimId);

		if (graph instanceof TimeGraph) {
			final TimeGraph timeGraph = (TimeGraph) graph;

			if (timeGraph.isValidSelection(hierarchyId, levelId)) {
				return timeGraph.getMembers(hierarchyId, levelId, start, end);
			} else {
				exceptionRegistry.throwException(DimensionModelException.class,
						1008, dimSelector);
				return null;
			}
		} else {
			exceptionRegistry.throwException(DimensionModelException.class,
					1007, dimId, TimeGraph.class.getSimpleName());
			return null;
		}
	}

	/**
	 * Creates a map of {@code DimensionGraph} instances for the specified
	 * {@code Collection}. The method looks for {@code Dimension} instances and
	 * converts those, all others are ignored and skipped.
	 * 
	 * @param dims
	 *            the collection to be transformed
	 * 
	 * @return the created map
	 */
	public Map<String, IDimensionGraph> createMap(final Collection<?> dims) {
		final Map<String, IDimensionGraph> dimensions = new HashMap<String, IDimensionGraph>();
		if (dims == null) {
			return dimensions;
		}

		for (final Object dim : dims) {
			if (dim instanceof IDimension) {
				final IDimension dimension = (IDimension) dim;
				final String id = dimension.getId();
				final IDimensionGraph graph = createGraph(dimension);

				if (dimensions.put(id, graph) != null) {
					exceptionRegistry.throwException(
							TidaDimensionHandlerException.class, 1002, id);
				}
			}
		}

		return dimensions;
	}

	/**
	 * Factory method for {@code DimensionGraph} instances.
	 * 
	 * @param dimension
	 *            the {@code Dimension} to create the {@code DimensionGraph} for
	 * 
	 * @return the created {@code DimensionGraph}
	 * 
	 * @throws TidaDimensionHandlerException
	 *             if a {@code DimensionGraph} cannot be created, e.g. if no
	 *             graph exists for the {@code Dimension}
	 */
	public IDimensionGraph createGraph(final IDimension dimension)
			throws TidaDimensionHandlerException {

		// factory to pick the correct type for the dimension
		try {
			final IDimensionGraph graph;

			if (dimension instanceof DescriptorDimension) {
				graph = new DescriptorGraph();
			} else if (dimension instanceof TimeDimension) {
				graph = new TimeGraph(intervalModel, granularityFactory,
						timeLevelTemplateManager);
			} else {
				final String type = dimension == null ? null : dimension
						.getClass().getSimpleName();
				exceptionRegistry.throwException(DimensionModelException.class,
						1006, type);
				return null;
			}

			// create the graph and format exceptions
			graph.create(dimension);

			return graph;
		} catch (final ForwardedRuntimeException e) {
			exceptionRegistry.throwRuntimeException(e);
			return null;
		}
	}

	/**
	 * Get the template for the specified identifier.
	 * 
	 * @param templateId
	 *            the identifier of the {@code TimeLevelTemplate}
	 * 
	 * @return the found template or {@code null} if not template exists
	 */
	public ITimeLevelTemplate getTimeLevelTemplate(final String templateId) {
		return timeLevelTemplateManager.getTemplate(granularityFactory,
				templateId);
	}
}
