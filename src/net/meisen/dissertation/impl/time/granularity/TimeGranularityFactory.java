package net.meisen.dissertation.impl.time.granularity;

import java.lang.reflect.Constructor;
import java.lang.reflect.Method;
import java.lang.reflect.Modifier;

import net.meisen.dissertation.config.xslt.DefaultValues;
import net.meisen.dissertation.exceptions.TimeGranularityFactoryException;
import net.meisen.dissertation.model.time.granularity.ITimeGranularity;
import net.meisen.dissertation.model.time.granularity.ITimeGranularityFactory;
import net.meisen.general.genmisc.exceptions.registry.IExceptionRegistry;
import net.meisen.general.genmisc.types.Classes;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;

/**
 * Factory used to create {@code TimeGranularity} instances or use created once
 * calling a static {@code instance}-method of a {@code TimeGranularity}-class.
 * 
 * @author pmeisen
 * 
 */
public class TimeGranularityFactory implements ITimeGranularityFactory {
	private final static String INSTANCEMETHOD = "instance";

	@Autowired
	@Qualifier(DefaultValues.EXCEPTIONREGISTRY_ID)
	private IExceptionRegistry exceptionRegistry;

	private boolean throwExceptions;

	/**
	 * Creates an instance of a {@code TimeGranularityFactory} which throws an
	 * exception if {@link #find(String)} or {@link #findClass(String)} would
	 * return {@code null}.
	 */
	public TimeGranularityFactory() {
		this(true);
	}

	/**
	 * Creates an instance of a {@code TimeGranularityFactory}. Depending on
	 * {@code throwExceptions} the factory will throw an exception if an
	 * instance cannot be created ({@code throwExceptions} must be {@code true}
	 * ), or just return {@code null} ({@code throwExceptions} must be
	 * {@code false}).
	 * 
	 * @param throwExceptions
	 *            {@code true} to throw an exception if an instance cannot be
	 *            created, otherwise {@code false}.
	 */
	public TimeGranularityFactory(final boolean throwExceptions) {
		this.setThrowExceptions(throwExceptions);
	}

	/**
	 * Finds a class which might be used to instantiate a
	 * {@code TimeGranularity} for the specified {@code timeGranularity}.
	 * 
	 * @param timeGranularity
	 *            the name of the {@code TimeGranularity}
	 * 
	 * @return the found {@code TimeGranularity}, will never return {@code null}
	 *         if {@link #isThrowExceptions()} is set to {@code true}
	 * 
	 * @throws TimeGranularityFactoryException
	 *             if exception should be thrown (i.e.
	 *             {@link #isThrowExceptions()} return {@code true}) and the
	 *             {@code timeGranularity} cannot be resolved
	 */
	protected Class<? extends ITimeGranularity> findClass(
			final String timeGranularity)
			throws TimeGranularityFactoryException {
		Class<? extends ITimeGranularity> clazz;

		// null will never have anything
		if (timeGranularity == null) {
			if (isThrowExceptions()) {
				exceptionRegistry.throwException(
						TimeGranularityFactoryException.class, 1000);
			}
			return null;
		}
		// check if we can resolve the class directly
		else if ((clazz = isValid(Classes.getClass(timeGranularity, false))) != null) {
			return clazz;
		}
		// check if we can find it in this package
		else if ((clazz = isValid(checkInPackage(getClass().getPackage(),
				timeGranularity))) != null) {
			return clazz;
		}
		// check if we can find it in the interface's package
		else if ((clazz = isValid(checkInPackage(
				ITimeGranularityFactory.class.getPackage(), timeGranularity))) != null) {
			return clazz;
		}
		// there is no hope
		else {
			if (isThrowExceptions()) {
				exceptionRegistry.throwException(
						TimeGranularityFactoryException.class, 1001,
						timeGranularity);
			}
			return null;
		}
	}

	@Override
	public ITimeGranularity find(final String timeGranularity)
			throws TimeGranularityFactoryException {
		final Class<? extends ITimeGranularity> clazz = findClass(timeGranularity);

		if (clazz == null) {
			return null;
		}

		// check if there is a method to instantiate
		for (final Method m : clazz.getMethods()) {
			try {
				if (m.getParameterTypes().length == 0
						&& "instance".equalsIgnoreCase(m.getName())
						&& ITimeGranularity.class.isAssignableFrom(m
								.getReturnType())
						&& Modifier.isStatic(m.getModifiers())) {
					return (ITimeGranularity) m.invoke(null);
				}
			} catch (final Exception e) {
				// ignore
			}
		}

		// check if there is a constructor to be execute
		for (final Constructor<?> c : clazz.getConstructors()) {
			try {
				if (c.getParameterTypes().length == 0) {
					return clazz.newInstance();
				}
			} catch (final Exception e) {
				// ignore
			}
		}

		if (isThrowExceptions()) {
			exceptionRegistry.throwException(
					TimeGranularityFactoryException.class, 1002,
					getInstancemethod(), clazz.getName());
		}
		return null;
	}

	/**
	 * Defines if exceptions should be thrown whenever something cannot be
	 * resolved.
	 * 
	 * @return {@code true} to throw exceptions, otherwise {@code null} is
	 *         returned
	 */
	public boolean isThrowExceptions() {
		return throwExceptions;
	}

	/**
	 * Defines if exceptions should be thrown whenever something cannot be
	 * resolved.
	 * 
	 * @param throwExceptions
	 *            {@code true} to throw exceptions, otherwise {@code null} is
	 *            returned
	 */
	public void setThrowExceptions(final boolean throwExceptions) {
		this.throwExceptions = throwExceptions;
	}

	/**
	 * Checks if the specified {@code clazz} is valid according to the needed
	 * type of {@code this}.
	 * 
	 * @param clazz
	 *            the {@code class} to be validated
	 * 
	 * @return the {@code clazz} or {@code null} if invalid
	 */
	protected Class<? extends ITimeGranularity> isValid(final Class<?> clazz) {
		if (clazz != null && ITimeGranularity.class.isAssignableFrom(clazz)) {

			@SuppressWarnings("unchecked")
			final Class<? extends ITimeGranularity> finalClazz = (Class<? extends ITimeGranularity>) clazz;
			return finalClazz;
		} else {
			return null;
		}
	}

	/**
	 * Searches for the specified class within the package
	 * 
	 * @param pack
	 *            the package to search for the class
	 * @param className
	 *            the name of the class (case insensitive)
	 * 
	 * @return the found class, or {@code null} if no class was found
	 */
	protected Class<?> checkInPackage(final Package pack, final String className) {
		if (className == null) {
			return null;
		}

		try {

			// check if the name fits and return the class if so
			for (final Class<?> clazz : Classes.getClasses(pack.getName())) {
				if (className.equalsIgnoreCase(clazz.getSimpleName())) {
					return clazz;
				}
			}

			return null;
		} catch (final Exception e) {
			return null;
		}
	}

	/**
	 * Gets the name of the method to instantiate something.
	 * 
	 * @return the name of the instantiate-method
	 */
	public String getInstancemethod() {
		return INSTANCEMETHOD;
	}
}
