package net.meisen.dissertation.impl.dimensions;

import java.util.Collection;
import java.util.HashSet;
import java.util.Set;

import net.meisen.dissertation.exceptions.DescriptorDimensionException;
import net.meisen.dissertation.model.descriptors.DescriptorModel;
import net.meisen.general.genmisc.exceptions.ForwardedRuntimeException;
import net.meisen.general.genmisc.types.Objects;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class DescriptorDimension {
	public final static String ROOT_LEVEL_ID = "*";
	public final static String ROOT_MEMBER_ID = "*";
	public final static String UNASSIGNED_LEVEL_ID = "";

	private final static Logger LOG = LoggerFactory
			.getLogger(DescriptorDimension.class);

	private final String id;
	private final String name;

	private final Set<String> sharedLevels;
	private final DescriptorHierarchyManager hierarchies;

	public DescriptorDimension(final DescriptorModel<?> descriptorModel) {
		this(descriptorModel, descriptorModel.getName());
	}

	public DescriptorDimension(final DescriptorModel<?> descriptorModel,
			final String name) {
		if (descriptorModel == null) {
			throw new ForwardedRuntimeException(
					DescriptorDimensionException.class, 1006, name);
		}
		
		this.id = descriptorModel.getId();
		this.name = name;

		this.sharedLevels = new HashSet<String>();
		this.hierarchies = new DescriptorHierarchyManager(this);

		addSharedLevel(ROOT_LEVEL_ID);
	}

	public DescriptorHierarchy addHierarchy(final String id)
			throws ForwardedRuntimeException {
		return addHierarchy(id, id);
	}

	public DescriptorHierarchy addHierarchy(final String id, final String name)
			throws ForwardedRuntimeException {
		return hierarchies.addHierarchy(id, name);
	}

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

	public boolean isSharedLevel(final String id) {
		return sharedLevels.contains(id);
	}

	public Collection<DescriptorHierarchy> getHierarchies() {
		return hierarchies.getHierarchies();
	}

	public String getId() {
		return id;
	}

	public String getName() {
		return name;
	}

	@Override
	public boolean equals(final Object obj) {
		if (obj == this) {
			return true;
		} else if (obj == null) {
			return false;
		} else if (obj instanceof DescriptorDimension) {
			final DescriptorDimension cmpDim = (DescriptorDimension) obj;
			return Objects.equals(getId(), cmpDim.getId());
		} else {
			return false;
		}
	}

	public DescriptorHierarchy getHierarchy(final String hierarchyId) {
		return hierarchies.getHierarchy(hierarchyId);
	}
}
