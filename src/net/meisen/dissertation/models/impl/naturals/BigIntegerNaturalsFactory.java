package net.meisen.dissertation.models.impl.naturals;

import java.math.BigInteger;
import java.util.Random;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;

import net.meisen.dissertation.exceptions.NaturalsFactoryException;
import net.meisen.dissertation.models.INaturalsFactory;
import net.meisen.general.genmisc.exceptions.registry.IExceptionRegistry;

/**
 * Factory used to create number within the {@code BigIntegerNaturals} range and
 * of the specified type.
 * 
 * @author pmeisen
 * 
 */
public class BigIntegerNaturalsFactory implements
		INaturalsFactory<BigIntegerNaturals> {
	private static final BigIntegerNaturals ZERO = new BigIntegerNaturals(
			BigInteger.ZERO);
	private static final BigIntegerNaturals ONE = new BigIntegerNaturals(
			BigInteger.ONE);

	@Autowired
	@Qualifier("exceptionRegistry")
	private IExceptionRegistry exceptionRegistry;

	@Override
	public BigIntegerNaturals getZero() {
		return ZERO;
	}

	@Override
	public BigIntegerNaturals getOne() {
		return ONE;
	}

	@Override
	public BigIntegerNaturals getMax() {
		return null;
	}

	@Override
	public BigIntegerNaturals generate(final Object representative) {
		if (representative instanceof BigInteger) {
			return new BigIntegerNaturals((BigInteger) representative);
		} else if (representative instanceof Long) {
			return new BigIntegerNaturals(
					BigInteger.valueOf((Long) representative));
		} else if (representative instanceof Integer) {
			return new BigIntegerNaturals(
					BigInteger.valueOf((Integer) representative));
		} else {
			exceptionRegistry.throwException(NaturalsFactoryException.class,
					1000, representative,
					BigIntegerNaturalsFactory.class.getName());
			return null;
		}
	}

	@Override
	public Class<BigIntegerNaturals> getNaturalsType() {
		return BigIntegerNaturals.class;
	}

	@Override
	public Class<?> getJavaType() {
		return BigInteger.class;
	}

	@Override
	public BigIntegerNaturals getRandom() {

		// generate a random value
		final Random rnd = new Random();
		final BigInteger random = new BigInteger(rnd.nextInt(), rnd);

		// return it
		return new BigIntegerNaturals(random);
	}
}
