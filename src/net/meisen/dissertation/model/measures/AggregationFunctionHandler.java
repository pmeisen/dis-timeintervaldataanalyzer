package net.meisen.dissertation.model.measures;

import java.util.Collection;
import java.util.HashMap;
import java.util.Map;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * Handler to manage the different aggregation-functions available.
 * 
 * @author pmeisen
 * 
 */
public class AggregationFunctionHandler {
	private final static Logger LOG = LoggerFactory
			.getLogger(AggregationFunctionHandler.class);

	private Map<String, IAggregationFunction> functions = new HashMap<String, IAggregationFunction>();

	/**
	 * Creates an empty handler, i.e. no functions are registered.
	 */
	public AggregationFunctionHandler() {
		this(null);
	}

	/**
	 * Creates an handler and registers the passed functions.
	 * 
	 * @param functions
	 *            the functions to be registered
	 */
	public AggregationFunctionHandler(
			final Collection<IAggregationFunction> functions) {
		if (LOG.isDebugEnabled()) {
			LOG.debug("Setting the aggregation functions to be: " + functions);
		}

		setFunctions(functions);
	}

	/**
	 * Resolves a function by it's {@code functionName}. The resolving is case
	 * insensitive, i.e. the case of the {@code functionName} does not matter.
	 * 
	 * @param functionName
	 *            the name of the function to be resolved
	 * 
	 * @return the function found for the specified name or {@code null} if not
	 *         function could be found
	 */
	public IAggregationFunction resolve(final String functionName) {
		if (functionName == null) {
			return null;
		}
		final IAggregationFunction func = functions.get(functionName
				.toLowerCase());
		
		if (func == null) {
			return null;
		} else {
			return func.create();
		}
	}

	/**
	 * Gets all the functions registered at the handler.
	 * 
	 * @return all the functions registered at the handler
	 */
	public Collection<IAggregationFunction> getFunctions() {
		return functions.values();
	}

	/**
	 * Specified the functions of the handler, i.e. all other functions are
	 * removed and only the passed once are kept.
	 * 
	 * @param functions
	 *            the functions to be registered at the handler
	 */
	public void setFunctions(final Collection<IAggregationFunction> functions) {
		// remove all functions
		this.functions.clear();

		// add the specified once
		addFunctions(functions);
	}

	/**
	 * Adds the specified functions to the handler, i.e. all function are kept
	 * and the specified {@code functions} are added. If a function with the
	 * same name is added, it overrides the actual one.
	 * 
	 * @param functions
	 *            the functions to be added
	 */
	public void addFunctions(final Collection<IAggregationFunction> functions) {
		for (final IAggregationFunction function : functions) {
			addFunction(function);
		}
	}

	/**
	 * Adds the specified function to the handler. If another function with the
	 * same name (see {@link IAggregationFunction#getName()}) is added it will
	 * be overridden.
	 * 
	 * @param function
	 *            the function to be registered
	 */
	public void addFunction(final IAggregationFunction function) {
		final IAggregationFunction old = this.functions.put(function.getName()
				.toLowerCase(), function);

		// log if something is overridden
		if (old != null) {
			if (LOG.isTraceEnabled()) {
				LOG.trace("Override the function '" + old.getName()
						+ "' of type '" + old.getClass().getName()
						+ "' with the implementation of type '"
						+ function.getClass().getName() + "'");
			}
		}
	}
}
