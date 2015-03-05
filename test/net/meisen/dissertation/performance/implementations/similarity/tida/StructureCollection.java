package net.meisen.dissertation.performance.implementations.similarity.tida;

import java.util.HashMap;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;

import net.meisen.dissertation.model.indexes.datarecord.slices.Bitmap;
import net.meisen.dissertation.performance.implementations.similarity.tida.TemporalRelation.Relation;

public class StructureCollection {
	private final Map<String, StructureGroup> groups;
	private final int maxPosition;
	private final IStructureGroupFactory factory;

	public StructureCollection(final int maxTimepoint) {
		this(new DefaultStructureGroupFactory(TouchingStructureGroup.class),
				maxTimepoint);
	}

	public StructureCollection(final IStructureGroupFactory factory,
			final int maxTimepoint) {
		this.groups = new HashMap<String, StructureGroup>();

		this.maxPosition = maxTimepoint;
		this.factory = factory;
	}

	protected StructureGroup getOrCreate(final String groupId) {
		StructureGroup group = groups.get(groupId);

		if (group == null) {
			group = factory.create(maxPosition);
			groups.put(groupId, group);
		}

		return group;
	}

	public int[] get(final String groupId, final int pos) {
		final StructureGroup group = groups.get(groupId);
		if (group == null) {
			return null;
		} else {
			return group.get(pos);
		}
	}

	public int[] set(final String groupId, final int pos, final Bitmap bitmap) {
		if (pos > maxPosition) {
			// TODO make it nice
			throw new IllegalStateException();
		}

		return getOrCreate(groupId).calc(pos, bitmap);
	}

	public void finish() {

		// get each group and trigger the last calculation
		for (final StructureGroup group : groups.values()) {
			group.handleLastBitmap();
		}
	}

	public Relation get(final int pos) {
		return Relation.values()[pos];
	}

	public Set<String> getGroups() {
		return groups.keySet();
	}
	
	public int getMaxPosition() {
		return maxPosition;
	}

	@Override
	public String toString() {
		final String nl = System.getProperty("line.separator");
		final StringBuilder sb = new StringBuilder();

		for (final Entry<String, StructureGroup> e : groups.entrySet()) {
			sb.append(e.getKey() + ": ");
			sb.append(e.getValue());
			sb.append(nl);
		}

		return sb.toString().trim();
	}
}