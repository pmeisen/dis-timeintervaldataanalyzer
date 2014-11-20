package net.meisen.dissertation.model.dimensions;

import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.Map;

import net.meisen.dissertation.exceptions.DescriptorDimensionException;
import net.meisen.general.genmisc.exceptions.ForwardedRuntimeException;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * Manager used to manage the different {@code DescriptorHierarchy} instances.
 * The manager manages the hierarchies of one specific
 * {@code DescriptorDimension}.
 * 
 * @author pmeisen
 * 
 */
public class DescriptorHierarchyManager {
	private final static Logger LOG = LoggerFactory
			.getLogger(DescriptorHierarchyManager.class);

	private final DescriptorDimension descriptorDimension;
	private final Map<String, DescriptorHierarchy> hierarchies;

	/**
	 * Constructor specifying the {@code DescriptorDimension} the manager
	 * manages the hierarchies for.
	 * 
	 * @param descriptorDimension
	 *            the {@code DescriptorDimension} the manager belongs to
	 */
	public DescriptorHierarchyManager(
			final DescriptorDimension descriptorDimension) {
		this.descriptorDimension = descriptorDimension;

		this.hierarchies = new HashMap<String, DescriptorHierarchy>();
	}

	/**
	 * Adds a hierarchy to the manager. The manager creates the hierarchy if
	 * there is no other hierarchy using the specified {@code id}.
	 * 
	 * @param id
	 *            the identifier of the hierarchy to be added
	 * @param name
	 *            the name of the hierarchy to be added
	 * 
	 * @return the created and added hierarchy
	 * 
	 * @throws ForwardedRuntimeException
	 *             if an hierarchy with the specified identifier already exists
	 */
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

	/**
	 * Gets all the defined hierarchies.
	 * 
	 * @return all the defined hierarchies
	 */
	public Collection<DescriptorHierarchy> getHierarchies() {
		return Collections.unmodifiableCollection(hierarchies.values());
	}

	/**
	 * Gets the hierarchy with the specified {@code hierarchyId}.
	 * 
	 * @param hierarchyId
	 *            the identifier of the hierarchy to retrieve
	 * 
	 * @return the {@code DescriptorHierarchy} associated, can be {@code null}
	 *         if no such hierarchy exists
	 */
	public DescriptorHierarchy getHierarchy(final String hierarchyId) {
		return hierarchies.get(hierarchyId);
	}
}