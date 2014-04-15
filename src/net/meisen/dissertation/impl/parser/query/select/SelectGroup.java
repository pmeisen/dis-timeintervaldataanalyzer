package net.meisen.dissertation.impl.parser.query.select;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.Iterator;
import java.util.List;

import net.meisen.dissertation.model.descriptors.Descriptor;

public class SelectGroup implements Iterable<Descriptor<?, ?, ?>> {
	private final List<Descriptor<?, ?, ?>> descriptors = new ArrayList<Descriptor<?, ?, ?>>();

	public SelectGroup() {
		// nothing to do
	}

	public SelectGroup(final Descriptor<?, ?, ?>... descriptors) {
		append(descriptors);
	}

	public SelectGroup(final Collection<Descriptor<?, ?, ?>> descriptors) {
		append(descriptors);
	}

	public SelectGroup(final SelectGroup selectGroup,
			final Descriptor<?, ?, ?>... descriptors) {
		this.descriptors.addAll(selectGroup.descriptors);
		append(descriptors);
	}

	public SelectGroup(final SelectGroup selectGroup,
			final Collection<Descriptor<?, ?, ?>> descriptors) {
		this.descriptors.addAll(selectGroup.descriptors);
		append(descriptors);
	}

	public void append(final Descriptor<?, ?, ?>... descriptors) {
		if (descriptors == null) {
			return;
		}
		append(Arrays.asList(descriptors));
	}

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

	public int size() {
		return descriptors.size();
	}

	public Descriptor<?, ?, ?> getDescriptor(final int position) {
		return descriptors.get(position);
	}

	public List<Descriptor<?, ?, ?>> getDescriptors() {
		return Collections.unmodifiableList(descriptors);
	}

	@Override
	public String toString() {
		return descriptors.toString();
	}

	@Override
	public Iterator<Descriptor<?, ?, ?>> iterator() {
		return descriptors.iterator();
	}
}
