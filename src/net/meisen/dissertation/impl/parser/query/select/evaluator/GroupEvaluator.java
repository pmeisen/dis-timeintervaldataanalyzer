package net.meisen.dissertation.impl.parser.query.select.evaluator;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.Iterator;
import java.util.List;
import java.util.Set;

import net.meisen.dissertation.exceptions.GroupEvaluatorException;
import net.meisen.dissertation.impl.parser.query.DimensionSelector;
import net.meisen.dissertation.impl.parser.query.select.SelectQuery;
import net.meisen.dissertation.impl.parser.query.select.group.GroupExpression;
import net.meisen.dissertation.impl.parser.query.select.group.GroupFilter;
import net.meisen.dissertation.model.data.DimensionModel;
import net.meisen.dissertation.model.data.DimensionModel.IMemberFilter;
import net.meisen.dissertation.model.data.MetaDataModel;
import net.meisen.dissertation.model.data.TidaModel;
import net.meisen.dissertation.model.descriptors.Descriptor;
import net.meisen.dissertation.model.descriptors.DescriptorModel;
import net.meisen.dissertation.model.dimensions.DescriptorMember;
import net.meisen.dissertation.model.dimensions.graph.DescriptorGraph;
import net.meisen.dissertation.model.dimensions.graph.DescriptorGraphLevel;
import net.meisen.dissertation.model.dimensions.graph.IDimensionGraph;
import net.meisen.dissertation.model.indexes.BaseIndexFactory;
import net.meisen.dissertation.model.indexes.datarecord.TidaIndex;
import net.meisen.dissertation.model.indexes.datarecord.slices.Bitmap;
import net.meisen.dissertation.model.indexes.datarecord.slices.Slice;
import net.meisen.general.genmisc.exceptions.ForwardedRuntimeException;
import net.meisen.general.genmisc.types.Objects;

/**
 * A group evaluator is used to evaluate the part of a {@code group by} part of
 * a {@code select} query.
 * 
 * @author pmeisen
 * 
 * @see SelectQuery
 * 
 */
public class GroupEvaluator {

	/**
	 * An entry within a group.
	 * 
	 * @author pmeisen
	 * 
	 * @param <T>
	 *            the type of the entry's meta-information
	 */
	protected static class GroupEntry<T> {

		private final String id;
		private final String label;
		private final T meta;

		/**
		 * Entry of a group.
		 * 
		 * @param id
		 *            the string representing the id
		 * @param meta
		 *            some meta-information
		 */
		public GroupEntry(final String id, final T meta) {
			this(id, id, meta);
		}

		/**
		 * The constructor to create a entry for.
		 * 
		 * @param id
		 *            the identifier
		 * @param label
		 *            the label
		 * @param meta
		 *            the meta-information
		 */
		public GroupEntry(final String id, final String label, final T meta) {
			this.id = id;
			this.label = label;
			this.meta = meta;
		}

		@Override
		public String toString() {
			return getId() + " (" + getMeta() + ")";
		}

		@Override
		public int hashCode() {
			return Objects.generateHashCode(1, 7, getId(), getLabel(),
					getMeta());
		}

		@Override
		public boolean equals(final Object obj) {
			if (obj == this) {
				return true;
			} else if (obj == null) {
				return false;
			} else if (obj instanceof GroupEntry) {
				final GroupEntry<?> ge = (GroupEntry<?>) obj;
				return Objects.equals(getId(), ge.getId())
						&& Objects.equals(getLabel(), ge.getLabel())
						&& Objects.equals(getMeta(), ge.getMeta());
			} else {
				return false;
			}
		}

		/**
		 * Gets the meta-information.
		 * 
		 * @return the meta-information
		 */
		public T getMeta() {
			return meta;
		}

		/**
		 * Gets the label.
		 * 
		 * @return the label
		 */
		public String getLabel() {
			return label;
		}

		/**
		 * Gets the identifier.
		 * 
		 * @return the identifier
		 */
		public String getId() {
			return id;
		}
	}

	/**
	 * Helper class to store the descriptors which make up a specific group.
	 * 
	 * @author pmeisen
	 * 
	 */
	protected static class Group implements Iterable<GroupEntry<?>> {
		private final List<GroupEntry<?>> entries = new ArrayList<GroupEntry<?>>();

		/**
		 * Default constructor, creates an empty group.
		 */
		public Group() {
			// nothing to do
		}

		/**
		 * Constructor to create a group for the specified {@code entries}.
		 * 
		 * @param entries
		 *            the entries which make up the group
		 * 
		 * @see Descriptor F
		 */
		public Group(final GroupEntry<?>... entries) {
			append(entries);
		}

		/**
		 * Constructor to create a group for the specified {@code entries}.
		 * 
		 * @param entries
		 *            the entries which make up the group
		 * 
		 * @see Descriptor
		 */
		public Group(final Collection<GroupEntry<?>> entries) {
			append(entries);
		}

		/**
		 * Constructor to create a {@code group} based on another group.
		 * Additionally, other {@code entries} can be appended.
		 * 
		 * @param group
		 *            the group this instance is based on
		 * @param entries
		 *            additional entries to be added
		 * 
		 * @see Descriptor
		 */
		public Group(final Group group, final GroupEntry<?>... entries) {
			this.entries.addAll(group.entries);
			append(entries);
		}

		/**
		 * Constructor to create a {@code group} based on another group.
		 * Additionally, other {@code entries} can be appended.
		 * 
		 * @param group
		 *            the group this instance is based on
		 * @param descriptors
		 *            additional entries to be added
		 * 
		 * @see Descriptor
		 */
		public Group(final Group group,
				final Collection<GroupEntry<?>> descriptors) {
			this.entries.addAll(group.entries);
			append(descriptors);
		}

		/**
		 * Append the specified {@code entries} to the group.
		 * 
		 * @param entries
		 *            the entries to be appended
		 */
		public void append(final GroupEntry<?>... entries) {
			if (entries == null) {
				return;
			}
			append(Arrays.asList(entries));
		}

		/**
		 * Append the specified {@code entries} to the group.
		 * 
		 * @param entries
		 *            the entries to be appended
		 */
		public void append(final Collection<GroupEntry<?>> entries) {
			for (final GroupEntry<?> entry : entries) {
				if (entry == null) {
					throw new NullPointerException(
							"Null descriptors are not allowed.");
				}

				// add the descriptor
				this.entries.add(entry);
			}
		}

		/**
		 * Gets the amount of entries within the group.
		 * 
		 * @return the amount of entries within the group
		 */
		public int size() {
			return entries.size();
		}

		/**
		 * Get the entries at the specified {@code position} of the group.
		 * 
		 * @param position
		 *            the position to get the entry for (the position is
		 *            0-based)
		 * 
		 * @return the entry at the specified position of the group
		 */
		public GroupEntry<?> getEntry(final int position) {
			return entries.get(position);
		}

		/**
		 * Get the entries of the group.
		 * 
		 * @return the entries of the group
		 */
		public List<GroupEntry<?>> getEntries() {
			return Collections.unmodifiableList(entries);
		}

		/**
		 * Gets a single string representing the group.
		 * 
		 * @return a single string representing the group
		 */
		public String getString() {
			final StringBuffer sb = new StringBuffer();

			// create a list
			String separator = "";
			for (final GroupEntry<?> entry : entries) {
				sb.append(entry.getLabel());
				sb.append(separator);

				separator = ", ";
			}

			return sb.toString();
		}

		/**
		 * Transforms the list of entries into a list of strings, using the
		 * label of an entry (i.e. {@link GroupEntry#getLabel()}.
		 * 
		 * @return a list of strings representing the group
		 */
		public List<String> toStringList() {
			final List<String> list = new ArrayList<String>(entries.size());
			for (final GroupEntry<?> entry : entries) {
				list.add(entry.getLabel());
			}
			return list;
		}

		@Override
		public String toString() {
			return entries.toString();
		}

		@Override
		public Iterator<GroupEntry<?>> iterator() {
			return entries.iterator();
		}

		@Override
		public boolean equals(final Object o) {
			if (o == null) {
				return false;
			} else if (o == this) {
				return true;
			} else if (o instanceof Group) {
				final Group sg = (Group) o;
				return this.entries.equals(sg.entries);
			} else {
				return false;
			}
		}

		@Override
		public int hashCode() {
			return this.entries.hashCode();
		}
	}

	private final DimensionModel dimensionModel;
	private final MetaDataModel metaDataModel;
	private final TidaIndex index;
	private final BaseIndexFactory factory;

	/**
	 * Creates an evaluator to evaluate {@code GroupExpression} instances
	 * against the specified {@code model}.
	 * 
	 * @param model
	 *            the model the evaluation is based on
	 * 
	 * @see GroupExpression
	 * @see TidaModel
	 */
	public GroupEvaluator(final TidaModel model) {
		dimensionModel = model.getDimensionModel();
		metaDataModel = model.getMetaDataModel();
		index = model.getIndex();
		factory = model.getIndexFactory();
	}

	/**
	 * Method to evaluate a specific {@code GroupExpression}, i.e. to retrieve
	 * the {@code GroupResult} for the different defined groups.
	 * 
	 * @param groupExpression
	 *            the {@code GroupExpression} to retrieve the results for
	 * 
	 * @return the result of the evaluation
	 */
	public GroupResult evaluateGroupExpression(
			final GroupExpression groupExpression) {

		// get all the groups for the definition
		final List<Group> groups = generateGroups(groupExpression);

		// if no group is defined we return null as indicator
		if (groups == null) {
			return null;
		}

		// iterate over the groups and generate the results
		final GroupResult result = new GroupResult();
		for (final Group group : groups) {

			// get all the bitmaps of the slices of the group
			final List<Bitmap> bitmaps = new ArrayList<Bitmap>(group.size());
			for (final GroupEntry<?> e : group) {
				final Bitmap bitmap;
				final Object meta = e.getMeta();

				if (meta instanceof Descriptor) {
					bitmap = getDescriptorBitmap((Descriptor<?, ?, ?>) meta);
				} else if (meta instanceof DimensionSelector) {
					bitmap = getDimensionSelectorBitmap(
							(DimensionSelector) meta, e.getId());
				} else {
					throw new ForwardedRuntimeException(
							GroupEvaluatorException.class, 1002,
							meta == null ? null : meta.getClass()
									.getSimpleName());
				}

				/*
				 * A null indicates that there aren't any records for the group.
				 * If that is the case, we just clear the bitmap see the later
				 * handling, which will not add any value for the group.
				 */
				if (bitmap == null) {
					bitmaps.clear();
					break;
				} else {
					bitmaps.add(bitmap);
				}
			}

			/*
			 * if we have several bitmaps, combine those
			 */
			final int size = bitmaps.size();
			if (size > 1) {
				final Bitmap groupBitmap = Bitmap.and(factory,
						bitmaps.toArray());
				result.add(group.toStringList(), groupBitmap);
			}
			/*
			 * if we just have one bitmap create a copy of it, so that the
			 * result can work just by itself
			 */
			else if (size == 1) {
				result.add(group.toStringList(), bitmaps.get(0));
			}
			/*
			 * if we have no bitmap at all, the group doesn't have any data
			 */
			else {
				// so we don't add any empty bitmap here
				// result.add(group.toStringList(), factory.createBitmap());
			}
		}

		return result;
	}

	/**
	 * Gets the bitmap for the specified descriptor.
	 * 
	 * @param desc
	 *            the descriptor to get the bitmap for
	 * 
	 * @return the created bitmap
	 */
	protected Bitmap getDescriptorBitmap(final Descriptor<?, ?, ?> desc) {
		final Slice<?> slice = index.getMetaIndexDimensionSlice(
				desc.getModelId(), desc.getId());

		/*
		 * if a slice doesn't exist it means that there aren't any records
		 */
		if (slice == null) {
			return null;
		}
		/*
		 * get the bitmap of the slice
		 */
		else {
			return slice.getBitmap();
		}
	}

	/**
	 * Gets the bitmap defined by the specified {@code dimSelector}.
	 * 
	 * @param dimSelector
	 *            the selector of the level to look for the member
	 * @param memberId
	 *            the identifier of the member to create the bitmap for
	 * 
	 * @return the created bitmap
	 */
	protected Bitmap getDimensionSelectorBitmap(
			final DimensionSelector dimSelector, final String memberId) {
		return dimensionModel.getBitmap(dimSelector, false,
				new IMemberFilter() {

					@Override
					public boolean selectOnlyOne() {
						return true;
					}

					@Override
					public boolean accept(final DescriptorMember member) {
						return memberId.equals(member.getId());
					}
				});
	}

	/**
	 * Generates the groups defined by the {@code GroupExpression}. The method
	 * ensures that excluded group members are excluded.
	 * 
	 * @param groupExpression
	 *            the definition of the group
	 * 
	 * @return the {@code List} of generated groups
	 * 
	 * @see Group
	 */
	protected List<Group> generateGroups(final GroupExpression groupExpression) {
		final List<GroupFilter> inclusions = groupExpression.getInclusions();
		final List<GroupFilter> exclusions = groupExpression.getExclusions();

		// combine the groups
		List<Group> result = null;
		for (final Object selector : groupExpression.getSelectors()) {

			if (selector instanceof String) {
				result = appendDescriptorIdSelector((String) selector,
						inclusions, exclusions, result);
			} else if (selector instanceof DimensionSelector) {
				result = appendDimensionSelector((DimensionSelector) selector,
						inclusions, exclusions, result);
			} else {
				throw new ForwardedRuntimeException(
						GroupEvaluatorException.class, 1001, selector,
						selector == null ? null : selector.getClass()
								.getSimpleName());
			}

			// check if everything is filtered, nothing can be added
			if (result.size() == 0) {
				break;
			}
		}

		// null will be returned if nothing is defined for the group
		return result;
	}

	/**
	 * Appends the descriptors of the {@code DescriptorModel} specified by the
	 * passed id.
	 * 
	 * @param descModelId
	 *            the identifier of the {@code DescriptorModel} to append the
	 *            descriptors for
	 * @param inclusions
	 *            the definitions of groups to be included
	 * @param exclusions
	 *            the definitions of groups to be excluded
	 * @param result
	 *            the current result from a previous creation
	 * 
	 * @return the new result containing the new groups
	 */
	protected List<Group> appendDescriptorIdSelector(final String descModelId,
			final List<GroupFilter> inclusions,
			final List<GroupFilter> exclusions, final List<Group> result) {
		final List<Group> iterationResult = new ArrayList<Group>();

		// get the DescriptorModel
		final DescriptorModel<?> descModel = metaDataModel
				.getDescriptorModel(descModelId);
		if (descModel == null) {
			throw new ForwardedRuntimeException(GroupEvaluatorException.class,
					1000, descModelId);
		}

		if (result == null) {
			for (final Descriptor<?, ?, ?> desc : descModel.getAllDescriptors()) {

				// create the group and add it if not excluded already
				final GroupEntry<Descriptor<?, ?, ?>> entry = new GroupEntry<Descriptor<?, ?, ?>>(
						desc.getUniqueString(), desc);
				final Group group = new Group(entry);
				if (includes(group, inclusions) && !excludes(group, exclusions)) {
					iterationResult.add(new Group(entry));
				}
			}
		} else {
			for (final Group selectGroup : result) {
				for (final Descriptor<?, ?, ?> desc : descModel
						.getAllDescriptors()) {
					final GroupEntry<Descriptor<?, ?, ?>> entry = new GroupEntry<Descriptor<?, ?, ?>>(
							desc.getUniqueString(), desc);

					// create a new group with the appended value
					final Group copiedGroup = new Group(selectGroup, entry);

					/*
					 * append the new descriptor and check only the new value
					 * for exclusion
					 */
					if (includes(copiedGroup, inclusions)
							&& !excludes(copiedGroup, exclusions)) {
						iterationResult.add(copiedGroup);
					}
				}
			}
		}

		return iterationResult;
	}

	/**
	 * Appends the members of the {@code DimensionSelector} specified by the
	 * passed id.
	 * 
	 * @param selector
	 *            the {@code DimensionSelector} to append the members for
	 * @param inclusions
	 *            the definitions of groups to be included
	 * @param exclusions
	 *            the definitions of groups to be excluded
	 * @param result
	 *            the current result from a previous creation
	 * 
	 * @return the new result containing the new groups
	 */
	protected List<Group> appendDimensionSelector(
			final DimensionSelector selector,
			final List<GroupFilter> inclusions,
			final List<GroupFilter> exclusions, final List<Group> result) {

		final IDimensionGraph dimGraph = dimensionModel.getDimension(selector
				.getDimensionId());
		if (dimGraph == null) {
			throw new ForwardedRuntimeException(GroupEvaluatorException.class,
					1003, selector.getDimensionId());
		} else if (dimGraph instanceof DescriptorGraph == false) {
			throw new ForwardedRuntimeException(GroupEvaluatorException.class,
					1004, selector.getDimensionId());
		}

		final List<Group> iterationResult = new ArrayList<Group>();

		final DescriptorGraph descDimGraph = (DescriptorGraph) dimGraph;
		final DescriptorGraphLevel descriptorGraphLevel = descDimGraph
				.getLevel(selector.getHierarchyId(), selector.getLevelId());
		if (descriptorGraphLevel == null) {
			if (dimGraph.getDimension().hasHierarchy(selector.getHierarchyId())) {
				throw new ForwardedRuntimeException(
						GroupEvaluatorException.class, 1006,
						selector.getLevelId());
			} else {
				throw new ForwardedRuntimeException(
						GroupEvaluatorException.class, 1005,
						selector.getHierarchyId());
			}
		}
		final Set<DescriptorMember> members = descriptorGraphLevel
				.getMembers(selector.getHierarchyId());

		if (result == null) {
			for (final DescriptorMember member : members) {
				final GroupEntry<DimensionSelector> entry = new GroupEntry<DimensionSelector>(
						member.getId(), member.getName(), selector);
				final Group group = new Group(entry);

				if (includes(group, inclusions) && !excludes(group, exclusions)) {
					iterationResult.add(new Group(entry));
				}
			}
		} else {
			for (final Group selectGroup : result) {
				for (final DescriptorMember member : members) {
					final GroupEntry<DimensionSelector> entry = new GroupEntry<DimensionSelector>(
							member.getId(), member.getName(), selector);

					// create a new group with the appended value
					final Group copiedGroup = new Group(selectGroup, entry);

					/*
					 * append the new descriptor and check only the new value
					 * for exclusion
					 */
					if (includes(copiedGroup, inclusions)
							&& !excludes(copiedGroup, exclusions)) {
						iterationResult.add(copiedGroup);
					}
				}
			}
		}

		return iterationResult;
	}

	/**
	 * Validates if the specified group should be included using the specified
	 * {@code inclusions}.
	 * 
	 * @param group
	 *            the group to be checked
	 * @param inclusions
	 *            the defined inclusions
	 * 
	 * @return {@code true} if it should be included, otherwise {@code false}
	 */
	protected boolean includes(final Group group,
			final List<GroupFilter> inclusions) {

		// no inclusion means include
		if (inclusions == null || inclusions.size() == 0) {
			return true;
		}

		// check each defined inclusion
		for (final GroupFilter inclusion : inclusions) {
			if (isSelected(inclusion, group)) {
				return true;
			}
		}

		return false;
	}

	/**
	 * Checks if the specified {@code group} should be excluded from the group.
	 * 
	 * @param group
	 *            the group to be checked
	 * @param exclusions
	 *            the defined exclusions to be tested against
	 * 
	 * @return {@code true} if it has to be excluded, otherwise {@code false}
	 * 
	 * @see GroupFilter
	 */
	protected boolean excludes(final Group group,
			final List<GroupFilter> exclusions) {

		// check each defined exclusion
		for (final GroupFilter exclusion : exclusions) {
			if (isSelected(exclusion, group)) {
				return true;
			}
		}

		return false;
	}

	/**
	 * Checks if the specified filter selects the specified {@code group}.
	 * 
	 * @param filter
	 *            the filter to be checked
	 * @param group
	 *            the group to check
	 * 
	 * @return {@code true} if the filter selects the group, otherwise
	 *         {@code false}
	 */
	protected boolean isSelected(final GroupFilter filter, final Group group) {
		final int groupSize = group.size();

		for (int i = 0; i < groupSize; i++) {

			/*
			 * check if the exclusion can be fulfilled by the group, otherwise
			 * we can skip it directly
			 */
			if (filter.getAmountOfValues() > groupSize) {
				continue;
			}

			// get the descriptor and the string of it
			final GroupEntry<?> entry = group.getEntry(i);
			final String uniqueString = entry.getId();

			/*
			 * if we don't match the exclusion we can stop here
			 */
			if (!filter.getValue(i).matches(uniqueString)) {
				break;
			}
			/*
			 * check if we reached the end of the exclusion, if so we reached
			 * the end:
			 * 
			 * Example: group = ("Apple", "Philipp") and exclusion = ("A*") the
			 * exclusion matches and there are not more values defined,
			 * therefore it has to excluded.
			 */
			else if (filter.getAmountOfValues() == i + 1) {
				return true;
			}
		}

		return false;
	}
}
