package net.meisen.dissertation.impl.dataretriever;

import java.util.Random;
import java.util.UUID;

/**
 * An entry within a {@code FixedStructureDataRetrieverConfig} used to define a
 * field to be available within the {@code FixedStructureDataCollection}.
 * 
 * @author pmeisen
 * 
 */
public class FixedStructureDataRetrieverConfigEntry {
	private static final Random RND = new Random();

	private String name;
	private Class<?> type;
	private Boolean random;
	private Object value;

	/**
	 * Constructor to create an invalid entry. Invalid because
	 * {@link #setName(String)} and {@link #setType(Class)} must be called,
	 * until than the entry is incomplete and not usable.
	 */
	public FixedStructureDataRetrieverConfigEntry() {
		this(null, null, null, null);
	}

	/**
	 * Constructor used to create a valid entry for a
	 * {@code FixedStructureDataRetrieverConfig}. The entry will generate random
	 * values of the specified type (as long as {@link #setRandom(boolean)} or
	 * {@link #setValue(Object)} is not called to modify this behavior).
	 * 
	 * @param name
	 *            the name of the field within the
	 *            {@code FixedStructureDataRetrieverConfig}
	 * @param type
	 *            the type of the data retrieved in the field
	 */
	public FixedStructureDataRetrieverConfigEntry(final String name,
			final Class<?> type) {
		this(name, type, null, null);
	}

	/**
	 * Constructor to create an entry with the specified {@code name}, of the
	 * specified {@code type} and with the specified fixed {@code value}.
	 * 
	 * @param name
	 *            the name of the field within the
	 *            {@code FixedStructureDataRetrieverConfig}
	 * @param type
	 *            the type of the data retrieved in the field
	 * @param value
	 *            the value which must be assignable to specified type
	 */
	public FixedStructureDataRetrieverConfigEntry(final String name,
			final Class<?> type, final Object value) {
		this(name, type, value, null);
	}

	/**
	 * Constructor to create an entry with the specified {@code name} and the
	 * specified {@code value}.
	 * 
	 * @param name
	 *            the name of the field within the
	 *            {@code FixedStructureDataRetrieverConfig}
	 * @param value
	 *            the value which cannot be {@code null}, if {@code null} the
	 *            entry is an invalid one and can only be validated calling the
	 *            {@link #setType(Class)} method
	 */
	public FixedStructureDataRetrieverConfigEntry(final String name,
			final Object value) {
		this(name, null, value, null);
	}

	/**
	 * Constructor to create an entry with the specified {@code name} and of the
	 * specified {@code type}. If {@code value} is not {@code null} and
	 * {@code random} is {@code false} the value will be assumed to be fixed and
	 * used for every record of {@code this} entry. Otherwise, i.e.
	 * {@code random} is {@code true} or {@code value} is {@code null}, a random
	 * value will be generated for each record of {@code this} entry.
	 * 
	 * @param name
	 *            the name of the field within the
	 *            {@code FixedStructureDataRetrieverConfig}
	 * @param type
	 *            the type of the data retrieved in the field
	 * @param value
	 *            the value which must be assignable to specified type
	 * @param random
	 *            {@code true} to create random values for each record of
	 *            {@code this} entry, otherwise {@code false}
	 */
	public FixedStructureDataRetrieverConfigEntry(final String name,
			final Class<?> type, final Object value, final Boolean random) {

		// make sure we have a valid combination
		if (value != null && type != null
				&& !type.isAssignableFrom(value.getClass())) {
			throw new IllegalArgumentException("The specified value '" + value
					+ "' is not assignable from the type '" + type.getName()
					+ "'");
		}

		// set the specified values
		this.name = name;
		this.type = type == null && value != null ? value.getClass() : type;
		this.value = value;
		this.random = random;
	}

	/**
	 * Gets the type of the entry.
	 * 
	 * @return the type of the entry
	 */
	public Class<?> getType() {
		return type;
	}

	/**
	 * Sets the type of the entry, cannot be {@code null}.
	 * 
	 * @param type
	 *            the type of the entry, cannot be {@code null}
	 */
	public void setType(final Class<?> type) {
		this.type = type;
	}

	/**
	 * Gets the name of the entry.
	 * 
	 * @return the name of the entry
	 */
	public String getName() {
		return name;
	}

	/**
	 * Sets the name of the entry, cannot be {@code null}.
	 * 
	 * @param name
	 *            the name of the entry, cannot be {@code null}
	 */
	public void setName(final String name) {
		this.name = name;
	}

	/**
	 * Gets the defined value of {@code this} entry.
	 * 
	 * @return the defined value of {@code this} entry
	 */
	public Object getValue() {
		return value;
	}

	/**
	 * Sets the value, if set to {@code null} and {@code random} is not
	 * specified the value will be created randomly. To force the {@code null}
	 * value to be returned by {@link #createValue()} it is necessary to set
	 * {@code random} to {@code false}.
	 * 
	 * @param value
	 *            the value to be used
	 */
	public void setValue(final Object value) {
		this.value = value;
	}

	/**
	 * Determines if the values for {@code this} entry are created randomly or
	 * are pre-defined (i.e. fixed).
	 * 
	 * @return {@code true} if another value is created every time
	 *         {@link #createValue()} is called, otherwise {@code false}
	 */
	public boolean isRandom() {
		if (this.random == null) {
			return (getValue() == null);
		} else {
			return this.random;
		}
	}

	/**
	 * Defines if the created values should be randomly created or fixed (i.e.
	 * retrieved from {@link #getValue()}.
	 * 
	 * @param random
	 *            {@code true} to create random values, otherwise {code false}
	 */
	public void setRandom(final boolean random) {
		this.random = random;
	}

	/**
	 * Create a fixed or random value for a record of {@code this} entry.
	 * 
	 * @return the created value
	 */
	public Object createValue() {

		if (isRandom()) {
			if (type.equals(Integer.class)) {
				return RND.nextInt();
			} else if (type.equals(Long.class)) {
				return RND.nextLong();
			} else if (type.equals(Double.class)) {
				return Double.MAX_VALUE * RND.nextDouble();
			} else if (type.equals(Boolean.class)) {
				return RND.nextBoolean();
			} else if (type.equals(String.class)) {
				return UUID.randomUUID().toString();
			} else {
				throw new IllegalArgumentException("The type '"
						+ type.getName()
						+ "' is not supported for random values.");
			}
		} else {
			return getValue();
		}
	}
}
