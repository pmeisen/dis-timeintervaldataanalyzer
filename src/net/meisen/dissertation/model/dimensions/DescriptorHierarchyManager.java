package net.meisen.dissertation.model.dimensions;

import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.Map;

import net.meisen.dissertation.exceptions.DescriptorDimensionException;
import net.meisen.general.genmisc.exceptions.ForwardedRuntimeException;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class DescriptorHierarchyManager {
	private final static Logger LOG = LoggerFactory
			.getLogger(DescriptorHierarchyManager.class);

	private final DescriptorDimension descriptorDimension;

	private final Map<String, DescriptorHierarchy> hierarchies;

	public DescriptorHierarchyManager(
			final DescriptorDimension descriptorDimension) {
		this.descriptorDimension = descriptorDimension;

		this.hierarchies = new HashMap<String, DescriptorHierarchy>();
	}

	public DescriptorHierarchy addHierarchy(final String id, final String name)
			throws ForwardedRuntimeException {

		// create the hierarchy instance
		final DescriptorHierarchy hierarchy = new DescriptorHierarchy(
				descriptorDimension, id, name);

		// add the hierarchy
		if (hierarchies.containsKey(id)) {
			throw new ForwardedRuntimeException(
					DescriptorDimensionException.class, 1000, hierarchy);
		} else {
			if (LOG.isTraceEnabled()) {
				LOG.trace("Adding hierarchy with id '" + id + "' and name '"
						+ name + "'.");
			}

			hierarchies.put(id, hierarchy);
			return hierarchy;
		}
	}

	public Collection<DescriptorHierarchy> getHierarchies() {
		return Collections.unmodifiableCollection(hierarchies.values());
	}

	public DescriptorHierarchy getHierarchy(final String hierarchyId) {
		return hierarchies.get(hierarchyId);
	}
}