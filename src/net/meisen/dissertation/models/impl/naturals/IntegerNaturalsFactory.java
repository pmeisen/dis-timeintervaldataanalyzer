package net.meisen.dissertation.models.impl.naturals;

import java.util.Random;

import net.meisen.dissertation.exceptions.NaturalsFactoryException;
import net.meisen.dissertation.models.INaturalsFactory;
import net.meisen.general.genmisc.exceptions.registry.IExceptionRegistry;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;

/**
 * Factory used to create number within the {@code IntegerNaturals} range and of
 * the specified type.
 * 
 * @author pmeisen
 * 
 */
public class IntegerNaturalsFactory implements
		INaturalsFactory<IntegerNaturals> {
	private static final IntegerNaturals ZERO = new IntegerNaturals(0);
	private static final IntegerNaturals ONE = new IntegerNaturals(1);
	private static final IntegerNaturals MAX = new IntegerNaturals(
			Integer.MAX_VALUE);

	@Autowired
	@Qualifier("exceptionRegistry")
	private IExceptionRegistry exceptionRegistry;

	@Override
	public IntegerNaturals getZero() {
		return ZERO;
	}

	@Override
	public IntegerNaturals getOne() {
		return ONE;
	}

	@Override
	public IntegerNaturals getMax() {
		return MAX;
	}

	@Override
	public IntegerNaturals generate(final Object representative) {
		if (representative instanceof Integer) {
			return new IntegerNaturals((Integer) representative);
		} else {
			exceptionRegistry.throwException(NaturalsFactoryException.class,
					1000, representative,
					IntegerNaturalsFactory.class.getName());
			return null;
		}
	}

	@Override
	public Class<IntegerNaturals> getNaturalsType() {
		return IntegerNaturals.class;
	}

	@Override
	public Class<?> getJavaType() {
		return Integer.class;
	}

	@Override
	public IntegerNaturals getRandom() {

		// create a random integer
		final Random rnd = new Random();
		final int random = rnd.nextInt();

		// return it
		return new IntegerNaturals(random);
	}
}