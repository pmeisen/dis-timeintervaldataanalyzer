package net.meisen.dissertation.model.dimensions;

import java.util.Collection;
import java.util.HashSet;
import java.util.Set;

import net.meisen.dissertation.exceptions.DescriptorDimensionException;
import net.meisen.dissertation.model.descriptors.DescriptorModel;
import net.meisen.general.genmisc.exceptions.ForwardedRuntimeException;
import net.meisen.general.genmisc.types.Objects;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * Dimension used for a specific {@code DescriptorModel}.
 * 
 * @author pmeisen
 * 
 */
public class DescriptorDimension implements IDimension {
	/**
	 * The id used for the root-level of every hierarchy of the dimension.
	 */
	public final static String ROOT_LEVEL_ID = "*";
	/**
	 * The id used for the one root member of the root-level.
	 */
	public final static String ROOT_MEMBER_ID = "*";
	/**
	 * The id used for an unassigned level, i.e. the level which contains
	 * members not assigned to anything.
	 */
	public final static String UNASSIGNED_LEVEL_ID = "";

	private final static Logger LOG = LoggerFactory
			.getLogger(DescriptorDimension.class);

	private final String id;
	private final String name;
	private final String descriptorModelId;

	private final Set<String> sharedLevels;
	private final DescriptorHierarchyManager hierarchies;

	/**
	 * Constructor to create a {@code DescriptorDimension} for the specified
	 * {@code DescriptorModel}.
	 * 
	 * @param descriptorModel
	 *            the model to create the dimension for
	 */
	public DescriptorDimension(final DescriptorModel<?> descriptorModel) {
		this(descriptorModel, descriptorModel == null ? null : descriptorModel
				.getName());
	}

	/**
	 * Constructor to create a {@code DescriptorDimension} for the specified
	 * {@code DescriptorModel} with the specified name.
	 * 
	 * @param descriptorModel
	 *            the model to create the dimension for
	 * @param name
	 *            the name of the dimension
	 */
	public DescriptorDimension(final DescriptorModel<?> descriptorModel,
			final String name) {
		this(descriptorModel == null ? null : descriptorModel.getId(),
				descriptorModel == null ? null : descriptorModel.getId(), name);
	}

	/**
	 * Constructor to create a {@code DescriptorDimension} with the specified
	 * {@code id} bound to the specified {@code descriptorModelId}.
	 * 
	 * @param id
	 *            the identifier of the dimension
	 * @param descriptorModelId
	 *            the identifier of the {@code DescriptorModel} the dimension
	 *            belongs to
	 */
	public DescriptorDimension(final String id, final String descriptorModelId) {
		this(id, descriptorModelId, null);
	}

	/**
	 * Constructor to create a {@code DescriptorDimension} with the specified
	 * {@code id}, bound to the specified {@code descriptorModelId} and the
	 * given {@code name}.
	 * 
	 * @param id
	 *            the identifier of the dimension
	 * @param descriptorModelId
	 *            the identifier of the {@code DescriptorModel} the dimension
	 *            belongs to
	 * @param name
	 *            the name of the dimension
	 */
	public DescriptorDimension(final String id, final String descriptorModelId,
			final String name) {
		if (id == null) {
			throw new ForwardedRuntimeException(
					DescriptorDimensionException.class, 1006, id);
		} else if (descriptorModelId == null) {
			throw new ForwardedRuntimeException(
					DescriptorDimensionException.class, 1021, descriptorModelId);
		}

		this.id = id;
		this.descriptorModelId = descriptorModelId;
		this.name = name;

		this.sharedLevels = new HashSet<String>();
		this.hierarchies = new DescriptorHierarchyManager(this);

		addSharedLevel(ROOT_LEVEL_ID);
	}

	/**
	 * Adds a new hierarchy to {@code this}. The hierarchy is created and
	 * returned if it does not exist.
	 * 
	 * @param id
	 *            the identifier of the hierarchy
	 * 
	 * @return the created and added hierarchy
	 * 
	 * @throws ForwardedRuntimeException
	 *             if the specified identifier is already used by another
	 *             hierarchy
	 */
	public DescriptorHierarchy addHierarchy(final String id)
			throws ForwardedRuntimeException {
		return addHierarchy(id, id);
	}

	/**
	 * Adds a new hierarchy to {@code this}. The hierarchy is created and
	 * returned if it does not exist.
	 * 
	 * @param id
	 *            the identifier of the hierarchy
	 * @param name
	 *            the name of the hierarchy to be added
	 * 
	 * @return the created and added hierarchy
	 * 
	 * @throws ForwardedRuntimeException
	 *             if the specified identifier is already used by another
	 *             hierarchy
	 */
	public DescriptorHierarchy addHierarchy(final String id, final String name)
			throws ForwardedRuntimeException {
		return hierarchies.addHierarchy(id, name);
	}

	/**
	 * Adds the specified level as shared level.
	 * 
	 * @param id
	 *            the level to be marked to be shared
	 * 
	 * @throws ForwardedRuntimeException
	 *             if the level with the specified identifier already is marked
	 *             to be shared
	 */
	public void addSharedLevel(final String id)
			throws ForwardedRuntimeException {

		if (sharedLevels.add(id)) {
			if (LOG.isTraceEnabled()) {
				LOG.trace("Marking level '" + id + "' as shared.");
			}
		} else {
			throw new ForwardedRuntimeException(
					DescriptorDimensionException.class, 1001, id);
		}
	}

	/**
	 * Checks if the specified level is marked to be shared.
	 * 
	 * @param id
	 *            the identifier of the level to be checked
	 * 
	 * @return {@code true} if the level is marked to be shared, otherwise
	 *         {@code false}
	 */
	public boolean isSharedLevel(final String id) {
		return sharedLevels.contains(id);
	}

	/**
	 * Gets all the defined hierarchies of the specified dimension.
	 * 
	 * @return a collection of all the hierarchies specified for {@code this}
	 */
	public Collection<DescriptorHierarchy> getHierarchies() {
		return hierarchies.getHierarchies();
	}

	@Override
	public String getId() {
		return id;
	}

	/**
	 * Gets the name to be used for {@code this}.
	 * 
	 * @return the name to be used for {@code this}
	 */
	public String getName() {
		return name == null ? getId() : name;
	}

	/**
	 * Gets the identifier of the model {@code this} is bound to.
	 * 
	 * @return the identifier of the {@code DescriptorModel} the dimension is
	 *         bound to
	 */
	public String getDescriptorModelId() {
		return descriptorModelId;
	}

	@Override
	public boolean equals(final Object obj) {
		if (obj == this) {
			return true;
		} else if (obj == null) {
			return false;
		} else if (obj instanceof DescriptorDimension) {
			final DescriptorDimension cmpDim = (DescriptorDimension) obj;
			return Objects.equals(getDescriptorModelId(),
					cmpDim.getDescriptorModelId())
					&& Objects.equals(getId(), cmpDim.getId());
		} else {
			return false;
		}
	}

	/**
	 * Gets the hierarchy of the dimension with the specified
	 * {@code hierarchyId}.
	 * 
	 * @param hierarchyId
	 *            the identifier of the hierarchy to be retrieved
	 * 
	 * @return the {@code DescriptorHierarchy}, might be {@code null} if no
	 *         hierarchy with the specified identifier exists
	 */
	public DescriptorHierarchy getHierarchy(final String hierarchyId) {
		return hierarchies.getHierarchy(hierarchyId);
	}

	@Override
	public String toString() {
		return getId() + (name != null ? " [" + getName() + "] " : "") + " ("
				+ getDescriptorModelId() + ")";
	}

	@Override
	public boolean hasHierarchy(final String hierarchyId) {
		return getHierarchy(hierarchyId) != null;
	}
}
