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
import net.meisen.dissertation.impl.parser.query.DimensionSelector;
import net.meisen.dissertation.model.descriptors.Descriptor;
import net.meisen.dissertation.model.descriptors.DescriptorModel;
import net.meisen.dissertation.model.dimensions.DescriptorMember;
import net.meisen.dissertation.model.dimensions.IDimension;
import net.meisen.dissertation.model.dimensions.graph.DescriptorDimensionGraph;
import net.meisen.dissertation.model.dimensions.graph.IDimensionGraph;
import net.meisen.dissertation.model.dimensions.graph.Level;
import net.meisen.dissertation.model.handler.TidaDimensionHandler;
import net.meisen.dissertation.model.indexes.BaseIndexFactory;
import net.meisen.dissertation.model.indexes.datarecord.TidaIndex;
import net.meisen.dissertation.model.indexes.datarecord.slices.Bitmap;
import net.meisen.dissertation.model.indexes.datarecord.slices.Slice;
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
	@Qualifier(DefaultValues.DIMENSIONHANDLER_ID)
	private TidaDimensionHandler dimensionHandler;

	@Autowired
	@Qualifier(DefaultValues.EXCEPTIONREGISTRY_ID)
	private IExceptionRegistry exceptionRegistry;

	@Autowired
	@Qualifier(DefaultValues.METADATAMODEL_ID)
	private MetaDataModel metaDataModel;

	@Autowired
	@Qualifier(DefaultValues.INDEXFACTORY_ID)
	private BaseIndexFactory indexFactory;

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
	 * Adds the specified dimensions to the {@code DimensionModel}. This should
	 * only be done before or during initialization. The map should be read only
	 * after the initialization is performed.
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

		addedDimensions = dimensions;
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
		final Map<String, IDimensionGraph> dims = this.dimensionHandler
				.createMap(addedDimensions);
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
		if (d instanceof DescriptorDimensionGraph == false) {
			exceptionRegistry.throwException(DimensionModelException.class,
					1003, dimId);
		}
		final DescriptorDimensionGraph dim = (DescriptorDimensionGraph) d;

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
		final Level level = dim.getLevel(hierarchyId, levelId);
		if (level == null) {
			exceptionRegistry.throwException(DimensionModelException.class,
					1005, dimId, hierarchyId, levelId);
		}

		// get all the members selected
		final Set<DescriptorMember> members = level.getMembers(hierarchyId);
		final Set<DescriptorMember> selectedMembers = new HashSet<DescriptorMember>();
		for (final DescriptorMember member : members) {

			if (filter == null || filter.accept(member)) {
				final Set<DescriptorMember> leafMembers = level.getLeafMembers(
						hierarchyId, member.getId());
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
}
