package net.meisen.dissertation.impl.indexes.mock;

import net.meisen.general.genmisc.types.Objects;

/**
 * A mock which can be used for testing indexes.
 * 
 * @author pmeisen
 * 
 */
public class PersonMock {
	private final String name;
	private final String firstName;
	private final int age;

	/**
	 * Constructor to defined {@code name}, {@code firstName} and {@code age} of
	 * the {@code Person}.
	 * 
	 * @param name
	 *            the name of the {@code Person}
	 * @param firstName
	 *            the first-name of the {@code Person}
	 * @param age
	 *            the age of the {@code Person}
	 */
	public PersonMock(final String name, final String firstName, final int age) {
		this.name = name;
		this.firstName = firstName;
		this.age = age;
	}

	/**
	 * Gets the name of the {@code Person}
	 * 
	 * @return the name of the {@code Person}
	 */
	public String getName() {
		return name;
	}

	/**
	 * Gets the first-name of the {@code Person}.
	 * 
	 * @return the first-name of the {@code Person}
	 */
	public String getFirstName() {
		return firstName;
	}

	/**
	 * Gets the age of the {@code Person}.
	 * 
	 * @return the age of the {@code Person}
	 */
	public int getAge() {
		return age;
	}

	@Override
	public int hashCode() {
		return Objects.generateHashCode(1, 7, name, firstName, age);
	}

	/**
	 * A {@code Person} is assumed to be equal if all attributes match.
	 */
	@Override
	public boolean equals(final Object o) {
		if (o == null || o instanceof PersonMock == false) {
			return false;
		} else {
			final PersonMock p = (PersonMock) o;
			return age == p.age && name.equals(p.name)
					&& firstName.equals(p.firstName);
		}
	}

	@Override
	public String toString() {
		return name + ", " + firstName + " (" + age + ")";
	}
}
