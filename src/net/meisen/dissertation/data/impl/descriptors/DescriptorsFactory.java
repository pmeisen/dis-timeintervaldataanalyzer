package net.meisen.dissertation.data.impl.descriptors;

import java.lang.reflect.Constructor;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import net.meisen.dissertation.config.xslt.DefaultValues;
import net.meisen.dissertation.data.IIdsFactory;
import net.meisen.dissertation.exceptions.DescriptorsFactoryException;
import net.meisen.dissertation.models.impl.data.Descriptor;
import net.meisen.dissertation.models.impl.data.DescriptorModel;
import net.meisen.general.genmisc.exceptions.registry.IExceptionRegistry;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;

public class DescriptorsFactory {

	@Autowired
	@Qualifier(DefaultValues.EXCEPTIONREGISTRY_ID)
	private IExceptionRegistry exceptionRegistry;

	private final IIdsFactory<?> idsFactory;

	private final Map<Class<?>, Class<? extends Descriptor<?, ?, ?>>> descriptors;

	public DescriptorsFactory(final IIdsFactory<?> idsFactory) {
		this(idsFactory, null);
	}

	public DescriptorsFactory(
			final IIdsFactory<?> idsFactory,
			final Map<Class<?>, Class<? extends Descriptor<?, ?, ?>>> descriptors) {
		this.idsFactory = idsFactory;
		this.descriptors = descriptors == null ? new HashMap<Class<?>, Class<? extends Descriptor<?, ?, ?>>>()
				: descriptors;
	}

	public Collection<Descriptor<?, ?, ?>> createDescriptors(
			final DescriptorModel model, final Collection<Object> values) {
		final List<Descriptor<?, ?, ?>> descriptors = new ArrayList<Descriptor<?, ?, ?>>();

		for (final Object value : values) {
			descriptors.add(createDescriptor(model, value));
		}

		return descriptors;
	}

	public Descriptor<?, ?, ?> createDescriptor(final DescriptorModel model,
			final Object value) {
		final Class<?> modelType = model.getDataType();
		final Class<?> valueType = value.getClass();

		// determine the right descriptor for the dataType
		final Class<? extends Descriptor<?, ?, ?>> clazz = getDescriptorClass(modelType);
		if (clazz == null) {
			exceptionRegistry.throwException(DescriptorsFactoryException.class,
					1002, modelType.getName());
			return null;
		}

		// get the constructor
		final Constructor<? extends Descriptor<?, ?, ?>> constructor = findConstructor(
				clazz, valueType);

		// create the instance and assign an id and a value
		final Descriptor<?, ?, ?> descriptor;
		try {
			descriptor = constructor.newInstance(model, idsFactory.getId(),
					value);
		} catch (final Exception e) {
			exceptionRegistry.throwException(DescriptorsFactoryException.class,
					1000, e, clazz.getName());
			return null;
		}

		return descriptor;
	}

	private Constructor<? extends Descriptor<?, ?, ?>> findConstructor(
			final Class<? extends Descriptor<?, ?, ?>> clazz,
			final Class<?> valueType) {

		Constructor<? extends Descriptor<?, ?, ?>> constructor;

		try {
			constructor = clazz.getConstructor(DescriptorModel.class,
					Object.class, valueType);
			return constructor;
		} catch (final NoSuchMethodException e) {
			constructor = null;

			// check if a default constructor is supported
			try {
				constructor = clazz.getConstructor(DescriptorModel.class,
						Object.class, Object.class);
			} catch (final NoSuchMethodException innerE) {
				constructor = null;
			}

			// if not throw the exception
			if (constructor == null) {
				exceptionRegistry.throwException(
						DescriptorsFactoryException.class, 1001, e,
						clazz.getName(), idsFactory.getIdClass().getName(),
						valueType.getName());
				return null;
			} else {
				return constructor;
			}
		}
	}

	protected Class<? extends Descriptor<?, ?, ?>> getDefaultDescriptorClass() {
		return descriptors.get(Object.class);
	}

	protected Class<? extends Descriptor<?, ?, ?>> getDescriptorClass(
			final Class<?> dataType) {
		final Class<? extends Descriptor<?, ?, ?>> clazz = descriptors
				.get(dataType);

		// check if we found a class, otherwise use the default
		if (clazz == null) {
			return getDefaultDescriptorClass();
		} else {
			return clazz;
		}
	}

	public Class<?> getIdClass() {
		return idsFactory.getIdClass();
	}
}
