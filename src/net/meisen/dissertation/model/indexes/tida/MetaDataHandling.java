package net.meisen.dissertation.model.indexes.tida;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import net.meisen.dissertation.model.descriptors.DescriptorModel;

/**
 * Defines how the meta-data should be handled if it cannot be found within the
 * specified {@code DescriptorModel}, when checking the data.
 * 
 * @author pmeisen
 * 
 */
public enum MetaDataHandling {
	/**
	 * The missing meta-data will be handled as {@code null}, this might lead to
	 * an error if {@code null}-values are not supported.
	 * 
	 * @see DescriptorModel#supportsNullDescriptor()
	 */
	HANDLEASNULL("null"),
	/**
	 * The missing meta-data will be created within the {@code DescriptorModel}.
	 */
	CREATEDESCRIPTOR("create", "add"),
	/**
	 * An error will be thrown
	 */
	FAILONERROR("fail");

	private final List<String> synonyms;

	private MetaDataHandling(final String... synonyms) {
		this.synonyms = new ArrayList<String>();
		this.synonyms.add(this.name());

		if (synonyms != null) {
			this.synonyms.addAll(Arrays.asList(synonyms));
		}
	}

	private boolean isSynonym(final String name) {
		return this.synonyms.contains(name);
	}

	/**
	 * Finds the {@code MetaDataHandling} associated to the specified
	 * {@code name}. If the name cannot be associated the default
	 * {@link #CREATEDESCRIPTOR} will be returned.
	 * 
	 * @param name
	 *            the name of the {@code MetaDataHandling} to retrieve
	 * 
	 * @return the {@code MetaDataHandling} associated
	 */
	public static MetaDataHandling find(final String name) {
		if (name != null) {
			for (final MetaDataHandling metaDataHandling : MetaDataHandling
					.values()) {
				if (metaDataHandling.isSynonym(name)) {
					return metaDataHandling;
				}
			}
		}

		return CREATEDESCRIPTOR;
	}
}
