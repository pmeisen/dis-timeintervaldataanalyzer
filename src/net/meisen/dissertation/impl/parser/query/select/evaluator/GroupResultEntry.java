package net.meisen.dissertation.impl.parser.query.select.evaluator;

import java.util.Collections;
import java.util.List;

import net.meisen.dissertation.model.indexes.datarecord.slices.Bitmap;
import net.meisen.general.genmisc.types.Strings;

/**
 * An entry, i.e. a specific group with it's bitmap, of a {@code GroupResult}.
 * 
 * @author pmeisen
 * 
 * @see GroupResult
 * 
 */
public class GroupResultEntry implements IBitmapResult {
	private final List<String> group;
	private final Bitmap bitmap;

	/**
	 * Constructor to create a {@code GroupResultEntry} for the specified
	 * {@code group} and the {@code bitmap} representing the result.
	 * 
	 * @param group
	 *            the group the result belongs to
	 * @param bitmap
	 *            the result
	 */
	public GroupResultEntry(final List<String> group, final Bitmap bitmap) {
		this.group = Collections.unmodifiableList(group);
		this.bitmap = bitmap;
	}

	/**
	 * Gets the group of the entry as single string.
	 * 
	 * @return the group of the entry
	 */
	public String getGroup() {
		return Strings.join(", ", group);
	}

	/**
	 * Gets the group of the entry as list, i.e. the different items making up
	 * the group.
	 * 
	 * @return the group of the entry
	 */
	public List<String> getGroupAsList() {
		return group;
	}

	@Override
	public Bitmap getBitmap() {
		return bitmap;
	}

	@Override
	public String toString() {
		return group.toString() + " (" + bitmap.determineCardinality() + ", "
				+ bitmap + ")";
	}

	/**
	 * Checks if the define is group is really a group or just a place-holder. A
	 * group must have at least one value defined that makes up the group,
	 * otherwise it is not assumed to be one.
	 * 
	 * @return {@code true} if the group is defined, i.e if at least one value
	 *         exists, otherwise {@code false}
	 */
	public boolean isGroup() {
		return group.size() > 0;
	}
}