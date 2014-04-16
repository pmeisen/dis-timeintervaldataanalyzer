package net.meisen.dissertation.impl.parser.query.select.evaluator;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.Iterator;
import java.util.List;

import net.meisen.dissertation.exceptions.GroupEvaluatorException;
import net.meisen.dissertation.impl.parser.query.select.SelectQuery;
import net.meisen.dissertation.impl.parser.query.select.logical.GroupExclusion;
import net.meisen.dissertation.impl.parser.query.select.logical.GroupExpression;
import net.meisen.dissertation.model.data.MetaDataModel;
import net.meisen.dissertation.model.data.TidaModel;
import net.meisen.dissertation.model.descriptors.Descriptor;
import net.meisen.dissertation.model.descriptors.DescriptorModel;
import net.meisen.dissertation.model.indexes.BaseIndexFactory;
import net.meisen.dissertation.model.indexes.datarecord.TidaIndex;
import net.meisen.dissertation.model.indexes.datarecord.bitmap.Bitmap;
import net.meisen.dissertation.model.indexes.datarecord.slices.IndexDimensionSlice;
import net.meisen.general.genmisc.exceptions.ForwardedRuntimeException;

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
	 * Helper class to store the descriptors which make up a specific group.
	 * 
	 * @author pmeisen
	 * 
	 */
	protected static class Group implements Iterable<Descriptor<?, ?, ?>> {
		private final List<Descriptor<?, ?, ?>> descriptors = new ArrayList<Descriptor<?, ?, ?>>();

		/**
		 * Default constructor, creates an empty group.
		 */
		public Group() {
			// nothing to do
		}

		/**
		 * Constructor to create a group for the specified {@code descriptors}.
		 * 
		 * @param descriptors
		 *            the descriptors which make up the group
		 * 
		 * @see Descriptor F
		 */
		public Group(final Descriptor<?, ?, ?>... descriptors) {
			append(descriptors);
		}

		/**
		 * Constructor to create a group for the specified {@code descriptors}.
		 * 
		 * @param descriptors
		 *            the descriptors which make up the group
		 * 
		 * @see Descriptor
		 */
		public Group(final Collection<Descriptor<?, ?, ?>> descriptors) {
			append(descriptors);
		}

		/**
		 * Constructor to create a {@code group} based on another group.
		 * Additionally, other {@code descriptors} can be appended.
		 * 
		 * @param group
		 *            the group this instance is based on
		 * @param descriptors
		 *            additional descriptors to be added
		 * 
		 * @see Descriptor
		 */
		public Group(final Group group,
				final Descriptor<?, ?, ?>... descriptors) {
			this.descriptors.addAll(group.descriptors);
			append(descriptors);
		}

		/**
		 * Constructor to create a {@code group} based on another group.
		 * Additionally, other {@code descriptors} can be appended.
		 * 
		 * @param group
		 *            the group this instance is based on
		 * @param descriptors
		 *            additional descriptors to be added
		 * 
		 * @see Descriptor
		 */
		public Group(final Group group,
				final Collection<Descriptor<?, ?, ?>> descriptors) {
			this.descriptors.addAll(group.descriptors);
			append(descriptors);
		}

		/**
		 * Append the specified {@code descriptors} to the group.
		 * 
		 * @param descriptors
		 *            the descriptors to be appended
		 */
		public void append(final Descriptor<?, ?, ?>... descriptors) {
			if (descriptors == null) {
				return;
			}
			append(Arrays.asList(descriptors));
		}

		/**
		 * Append the specified {@code descriptors} to the group.
		 * 
		 * @param descriptors
		 *            the descriptors to be appended
		 */
		public void append(final Collection<Descriptor<?, ?, ?>> descriptors) {
			for (final Descriptor<?, ?, ?> desc : descriptors) {
				if (desc == null) {
					throw new NullPointerException(
							"Null descriptors are not allowed.");
				}

				// add the descriptor
				this.descriptors.add(desc);
			}
		}

		/**
		 * Gets the amount of descriptors within the group.
		 * 
		 * @return the amount of descriptors within the group
		 */
		public int size() {
			return descriptors.size();
		}

		/**
		 * Get the descriptor at the specified {@code position} of the group.
		 * 
		 * @param position
		 *            the position to get the descriptor for (the position is
		 *            0-based)
		 * 
		 * @return the descriptors of the group
		 */
		public Descriptor<?, ?, ?> getDescriptor(final int position) {
			return descriptors.get(position);
		}

		/**
		 * Get the descriptors of the group.
		 * 
		 * @return the descriptors of the group
		 */
		public List<Descriptor<?, ?, ?>> getDescriptors() {
			return Collections.unmodifiableList(descriptors);
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
			for (final Descriptor<?, ?, ?> desc : descriptors) {
				sb.append(desc.getUniqueString());
				sb.append(separator);

				separator = ", ";
			}

			return sb.toString();
		}

		/**
		 * Transforms the list of descriptors into a list of strings, using the
		 * unique string of a descriptor (i.e.
		 * {@link Descriptor#getUniqueString()}.
		 * 
		 * @return a list of strings representing the group
		 */
		public List<String> toStringList() {
			final List<String> list = new ArrayList<String>(descriptors.size());
			for (final Descriptor<?, ?, ?> desc : descriptors) {
				list.add(desc.getUniqueString());
			}
			return list;
		}

		@Override
		public String toString() {
			return descriptors.toString();
		}

		@Override
		public Iterator<Descriptor<?, ?, ?>> iterator() {
			return descriptors.iterator();
		}

		@Override
		public boolean equals(final Object o) {
			if (o == null) {
				return false;
			} else if (o == this) {
				return true;
			} else if (o instanceof Group) {
				final Group sg = (Group) o;
				return this.descriptors.equals(sg.descriptors);
			} else {
				return false;
			}
		}

		@Override
		public int hashCode() {
			return this.descriptors.hashCode();
		}
	}

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
			for (final Descriptor<?, ?, ?> d : group.getDescriptors()) {
				final IndexDimensionSlice<?> slice = index
						.getMetaIndexDimensionSlice(d.getModelId(), d.getId());

				/*
				 * if a slice doesn't exist it means that there aren't any
				 * records
				 */
				if (slice == null) {
					bitmaps.clear();
					break;
				}
				/*
				 * get the bitmap of the slice
				 */
				else {
					bitmaps.add(slice.getBitmap());
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
			 * if we have no bitmap at all create an empty one
			 */
			else {
				result.add(group.toStringList(), factory.createBitmap());
			}
		}

		return result;
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
		final List<GroupExclusion> exclusions = groupExpression.getExclusions();

		// combine the groups
		List<Group> result = null;
		for (final String descId : groupExpression.getDescriptors()) {
			final List<Group> iterationResult = new ArrayList<Group>();

			// get the DescriptorModel
			final DescriptorModel<?> descModel = metaDataModel
					.getDescriptorModel(descId);
			if (descModel == null) {
				throw new ForwardedRuntimeException(
						GroupEvaluatorException.class, 1000, descId);
			}

			if (result == null) {
				for (final Descriptor<?, ?, ?> desc : descModel
						.getAllDescriptors()) {

					// create the group and add it if not excluded already
					final Group group = new Group(desc);
					if (!excludes(group, exclusions)) {
						iterationResult.add(new Group(desc));
					}
				}
			} else {
				for (final Group selectGroup : result) {
					for (final Descriptor<?, ?, ?> desc : descModel
							.getAllDescriptors()) {

						// create a new group with the appended value
						final Group copiedGroup = new Group(selectGroup, desc);

						/*
						 * append the new descriptor and check only the new
						 * value for exclusion
						 */
						if (!excludes(copiedGroup, exclusions)) {
							iterationResult.add(copiedGroup);
						}
					}
				}
			}

			// set the result for the next iteration
			result = iterationResult;

			// check if everything is filtered, nothing can be added
			if (result.size() == 0) {
				break;
			}
		}

		// null will be returned if nothing is defined for the group
		return result;
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
	 * @see GroupExclusion
	 */
	protected boolean excludes(final Group group,
			final List<GroupExclusion> exclusions) {
		int groupSize = group.size();

		// check each defined exclusion
		for (final GroupExclusion exclusion : exclusions) {

			for (int i = 0; i < groupSize; i++) {
				/*
				 * check if the exclusion can be fulfilled by the group,
				 * otherwise we can skip it directly
				 */
				if (exclusion.getAmountOfValues() > groupSize) {
					continue;
				}

				// get the descriptor and the string of it
				final Descriptor<?, ?, ?> descriptor = group.getDescriptor(i);
				final String uniqueString = descriptor.getUniqueString();

				/*
				 * if we don't match the exclusion we can stop here
				 */
				if (!exclusion.getValue(i).matches(uniqueString)) {
					break;
				}
				/*
				 * check if we reached the end of the exclusion, if so we
				 * reached the end:
				 * 
				 * Example: group = ("Apple", "Philipp") and exclusion = ("A*")
				 * the exclusion matches and there are not more values defined,
				 * therefore it has to excluded.
				 */
				else if (exclusion.getAmountOfValues() == i + 1) {
					return true;
				}
			}
		}

		return false;
	}
}
