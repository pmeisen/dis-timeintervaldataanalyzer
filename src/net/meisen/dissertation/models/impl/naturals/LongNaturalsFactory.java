package net.meisen.dissertation.models.impl.naturals;

import java.util.Random;

import net.meisen.dissertation.config.xslt.DefaultValues;
import net.meisen.dissertation.exceptions.NaturalsFactoryException;
import net.meisen.dissertation.models.INaturalsFactory;
import net.meisen.general.genmisc.exceptions.registry.IExceptionRegistry;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;

/**
 * Factory used to create number within the {@code LongNaturals} range and of
 * the specified type.
 * 
 * @author pmeisen
 * 
 */
public class LongNaturalsFactory implements INaturalsFactory<LongNaturals> {
	private static final LongNaturals ZERO = new LongNaturals(0l);
	private static final LongNaturals ONE = new LongNaturals(1l);
	private static final LongNaturals MAX = new LongNaturals(Long.MAX_VALUE);

	@Autowired
	@Qualifier(DefaultValues.EXCEPTIONREGISTRY_ID)
	private IExceptionRegistry exceptionRegistry;

	@Override
	public LongNaturals getZero() {
		return ZERO;
	}

	@Override
	public LongNaturals getOne() {
		return ONE;
	}

	@Override
	public LongNaturals getMax() {
		return MAX;
	}

	@Override
	public LongNaturals generate(final Object representative) {

		if (representative instanceof Long) {
			return new LongNaturals((Long) representative);
		} else if (representative instanceof Integer) {
			return new LongNaturals((Integer) representative);
		} else {
			exceptionRegistry.throwException(NaturalsFactoryException.class,
					1000, representative, LongNaturalsFactory.class.getName());
			return null;
		}
	}

	@Override
	public Class<LongNaturals> getNaturalsType() {
		return LongNaturals.class;
	}

	@Override
	public Class<?> getJavaType() {
		return Long.class;
	}

	@Override
	public LongNaturals getRandom() {
		// create a random integer
		final Random rnd = new Random();
		final long random = rnd.nextLong();

		// return it
		return new LongNaturals(random);
	}
}
