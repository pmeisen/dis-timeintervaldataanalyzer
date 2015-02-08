package net.meisen.dissertation.model.descriptors;

import net.meisen.dissertation.model.datasets.IDataRecord;
import net.meisen.general.genmisc.types.Objects;

/**
 * A descriptor is used to describe descriptive values within an
 * {@code RawTimeInterval}.<br/>
 * A Descriptor might define a nested static class of the type
 * {@link IDescriptorFactory}. The {@code DescriptorModel} will check for such a
 * public class and instantiate it once for each {@code Descriptor-Class}. The
 * factory is used to modify general descriptor specific aspects, e.g.
 * formatting of values. The class must be static and public.
 * 
 * @author pmeisen
 * 
 * @param <D>
 *            the type of the value of the descriptor
 * @param <T>
 *            the concrete {@code Descriptor} class
 * @param <I>
 *            the type of the identifier
 */
public abstract class Descriptor<D extends Object, T extends Descriptor<D, T, I>, I extends Object> {
	private final I id;
	private final DescriptorModel<I> model;

	private FactDescriptor<I> factDescriptor = null;

	/**
	 * Constructor which creates a {@code Descriptor} based on the specified
	 * {@code model} and with the specified {@code id}.
	 * 
	 * @param model
	 *            the {@code DescriptorModel} of the descriptor
	 * @param id
	 *            the identifier of the descriptor
	 */
	public Descriptor(final DescriptorModel<I> model, final I id) {
		this.id = id;
		this.model = model;
	}

	/**
	 * Gets the identifier of the descriptor.
	 * 
	 * @return the identifier of the descriptor
	 */
	public I getId() {
		return id;
	}

	/**
	 * Gets the {@code DescriptorModel} of this descriptor
	 * 
	 * @return the {@code DescriptorModel} of this descriptor
	 */
	protected DescriptorModel<I> getModel() {
		return model;
	}

	/**
	 * The name of the model used by this descriptor
	 * 
	 * @return the name of the model
	 */
	public String getModelName() {
		return model.getName();
	}

	/**
	 * Gets the identifier of the {@code DescriptorModel}.
	 * 
	 * @return the identifier of the {@code DescriptorModel}
	 */
	public String getModelId() {
		return getModel().getId();
	}

	/**
	 * Gets the class of the identifier used by this {@code Descriptor}. This
	 * class is equal to the generic-parameter {@code I}.
	 * 
	 * @return the class of the identifier used by this {@code Descriptor}
	 */
	public Class<?> getIdClass() {
		return getModel().getIdClass();
	}

	/**
	 * Gets the value of the {@code Descriptor}.
	 * 
	 * @return the value of the {@code Descriptor}
	 */
	public abstract D getValue();

	/**
	 * Gets the fact value for {@code this}.
	 * 
	 * @param record
	 *            the record to determine the fact value from
	 * 
	 * @return the fact value of the descriptor
	 */
	public abstract double getFactValue(final IDataRecord record);

	/**
	 * Defines if the {@link #getFactValue(IDataRecord)} is invariant
	 * considering the record. With other words, if {@code isRecordInvariant()}
	 * returns {@code true} the call {@code getFactValue(null)} is valid and
	 * won't throw any exception.
	 * 
	 * @return {@code true} if the fact is record invariant, i.e. does not
	 *         depend on a {@code record}, otherwise {@code false}
	 */
	public abstract boolean isRecordInvariant();

	/**
	 * Defines if the {@code Descriptor} is value invariant. If this method
	 * returns {@code true}, the value of the fact is equal for each
	 * {@code Descriptor} of the {@code DescriptorModel}.
	 * 
	 * @return {@code true} if the {@link #getFactValue(IDataRecord)} method
	 *         returns the same value for each {@code Descriptor} of the model,
	 *         otherwise {@code false}
	 */
	public abstract boolean isValueInvariant();

	/**
	 * Gets the {@code FactDescriptor} for {@code this}.
	 * 
	 * @return the {@code FactDescriptor} for {@code this}
	 */
	public FactDescriptor<I> getFactDescriptor() {

		// create a new FactDescriptor if we don't have one
		if (factDescriptor == null) {
			if (isValueInvariant()) {
				factDescriptor = new FactDescriptor<I>(getModelId(),
						getFactValue(null));
			} else if (isRecordInvariant()) {
				factDescriptor = new FactDescriptor<I>(getModelId(), getId(),
						getFactValue(null));
			} else {
				factDescriptor = new FactDescriptor<I>(getModelId(), getId());
			}
		}

		// return the FactDescriptor
		return factDescriptor;
	}

	/**
	 * Gets a unique string representation for the descriptor. This
	 * representation is necessary for queries, when filtering for the
	 * descriptor or inserting values.
	 * 
	 * @return the string representing this value
	 */
	public abstract String getUniqueString();

	@Override
	public abstract T clone();

	@Override
	public String toString() {
		return getClass().getSimpleName() + ": " + getUniqueString();
	}

	@Override
	public boolean equals(final Object o) {
		boolean cmp = false;

		if (o == this) {
			cmp = true;
		} else if (o == null) {
			// nothing to do
		} else if (getClass().equals(o.getClass())) {

			@SuppressWarnings("unchecked")
			final Descriptor<D, ?, ?> d = (Descriptor<D, ?, ?>) o;

			if (Objects.equals(getValue(), d.getValue())
					&& Objects.equals(getModel(), d.getModel())) {
				cmp = true;
			}
		}

		return cmp;
	}

	@Override
	public int hashCode() {
		return id.hashCode();
	}
}
