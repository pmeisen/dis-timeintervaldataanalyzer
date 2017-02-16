package net.meisen.dissertation.model.indexes;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.lang.reflect.Constructor;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.Map;

/**
 * The {@code IndexedCollectionDefinition} is used to define a
 * {@code IndexedCollection} used when building {@code IndexedCollection}
 * instances. The definition defines the {@code IndexedCollection} to be used
 * for a specific part of a {@code IndexKeyDefinition}.
 *
 * @author pmeisen
 * @see IIndexedCollection
 * @see IndexKeyDefinition
 */
public class IndexedCollectionDefinition {
    private final static Logger LOG = LoggerFactory.getLogger(IndexedCollectionDefinition.class);

    /**
     * A placeholder for the concrete {@code IndexKeyDefinition} when creating
     * the {@code IndexedCollection}. This placeholder should only be used once
     * within the defintion.
     */
    public static final Object INDEXKEYDEFINITION_PLACEHOLDER = new Object() {

        @Override
        public String toString() {
            return "IndexKeyDefinition";
        }
    };

    private final Object[] args;
    private final int placeHolderIdx;
    private final Class<?>[] types;
    private final Constructor<? extends IIndexedCollection> ctor;
    private final Class<? extends IIndexedCollection> type;

    /**
     * Constructor to create a {@code IndexedCollectionDefinition} using
     * specified arguments to be passed to the constructor.
     *
     * @param type the {@code IndexedCollection} to be created by {@code this}
     *             definition
     * @param args the arguments passed to the constructor, those arguments
     *             should include the {@link #INDEXKEYDEFINITION_PLACEHOLDER} if
     *             not the definition assumes that this is the first parameter of
     *             the {@code IndexedCollection}'s constructor
     */
    public IndexedCollectionDefinition(
            final Class<? extends IIndexedCollection> type,
            final Object... args) {
        this(type, null, args);
    }

    /**
     * Constructor to specify a {@code IndexedCollectionDefinition} by defining
     * the arguments and the types of the constructor to be used.
     *
     * @param type  the {@code IndexedCollection} to be created by {@code this}
     *              definition
     * @param types the forced types of the constructor, i.e. those types must
     *              match exactly the once of a constructor; a value can be
     *              {@code null} if so the argument-type will be used
     * @param args  the arguments passed to the constructor, those arguments
     *              should include the {@link #INDEXKEYDEFINITION_PLACEHOLDER} if
     *              not the definition assumes that this is the first parameter of
     *              the {@code IndexedCollection}'s constructor
     */
    public IndexedCollectionDefinition(
            final Class<? extends IIndexedCollection> type,
            final Class<?>[] types, final Object[] args) {
        if (type == null) {
            throw new NullPointerException("The type must be specified.");
        } else if (!IIndexedCollection.class.isAssignableFrom(type)) {
            throw new IllegalArgumentException(
                    "The type must be a extended implementation of a '"
                            + IIndexedCollection.class.getName() + "'.");
        }
        this.type = type;

        // create the arguments
        if (args == null || args.length == 0) {
            this.args = new Object[]{INDEXKEYDEFINITION_PLACEHOLDER};
            this.placeHolderIdx = 0;
        } else {
            int placeHolderIdx = -1;
            for (int i = 0; i < args.length; i++) {
                final Object arg = args[i];
                if (INDEXKEYDEFINITION_PLACEHOLDER.equals(arg)) {
                    if (placeHolderIdx != -1) {
                        throw new IllegalArgumentException("");
                    }
                    placeHolderIdx = i;
                }
            }
            if (placeHolderIdx == -1) {
                this.placeHolderIdx = 0;
                this.args = new Object[args.length + 1];
                this.args[0] = INDEXKEYDEFINITION_PLACEHOLDER;
                System.arraycopy(args, 0, this.args, 1, args.length);
            } else {
                this.placeHolderIdx = placeHolderIdx;
                this.args = args;
            }
        }

        // set the type
        this.types = types == null ? new Class<?>[]{} : types;

        if (this.args.length < this.types.length) {
            throw new IllegalArgumentException("There cannot be more types ('"
                    + this.types.length + "') specified than arguments ('"
                    + this.args.length + "') passed");
        } else if (this.types.length > this.placeHolderIdx
                && this.types[this.placeHolderIdx] != null
                && !IndexKeyDefinition.class.isAssignableFrom(this.types[this.placeHolderIdx])) {
            throw new IllegalArgumentException("The type at position '"
                    + this.placeHolderIdx + "' must be '"
                    + IndexKeyDefinition.class.getName()
                    + "' or 'null' but is '"
                    + this.types[this.placeHolderIdx].getName() + "'.");
        }

        // get the constructor to be used
        @SuppressWarnings("unchecked")
        final Constructor<? extends IIndexedCollection>[] ctors =
                (Constructor<? extends IIndexedCollection>[]) type.getConstructors();
        Map<Integer, Constructor<? extends IIndexedCollection>> usableCtors = new HashMap<>();
        for (final Constructor<? extends IIndexedCollection> ctor : ctors) {
            final int weight = checkConstructor(ctor);
            if (weight > 0) {
                if (usableCtors.put(weight, ctor) != null) {
                    LOG.warn("Found multiple equally weighted constructors for: " + Arrays.toString(this.args));
                }
            }
        }
        if (usableCtors.size() == 0) {
            throw new IllegalArgumentException(
                    "Cannot find any valid constructor for the definition.");
        }
        final int max = Collections.max(usableCtors.keySet());
        this.ctor = usableCtors.get(max);
    }

    /**
     * Checks if the specified {@code Constructor} can be used to create the
     * specified instance. The {@code ctor} must match (i.e. the arguments must
     * be assignable) the types of the arguments or must be equal to the
     * specified types (if any are defined).
     *
     * @param ctor the {@code Constructor} to be checked
     * @return {@code true} if the {@code ctor} can be used, otherwise
     * {@code false}
     */
    protected int checkConstructor(final Constructor<? extends IIndexedCollection> ctor) {
        final Class<?>[] pTypes = ctor.getParameterTypes();

        // the parameters should match
        if (args.length != pTypes.length) {
            return 0;
        }

        // we have a perfect match so far
        int weight = 2;

        // check the parameter-types
        for (int i = 0; i < args.length; i++) {
            final Object arg = args[i];
            if (arg == null) {
                continue;
            }

            final Class<?> pType = pTypes[i];
            final Class<?> type = getParameterType(i);
            final boolean exact = types.length > i && types[i] != null;

            // check if it fits
            if (exact) {
                // we need an exact match, there is no assignable
                weight = Math.min(pType.equals(type) ? 2 : 0, weight);
            } else {
                if (pType.equals(type)) {
                    weight = Math.min(2, weight);
                } else if (pType.isAssignableFrom(type)) {
                    weight = Math.min(1, weight);
                } else {

                    // cannot get worse, we can stop here
                    return 0;
                }
            }
        }

        return weight;
    }

    /**
     * Specifies the type of the parameter at the i-th position. The returned
     * type depends on the {@code placeHolderIdx}, if types are defined or the
     * arguments specified.
     *
     * @param i the position the get the parameter type for
     * @return the type of the parameter at the {@code i}-th position
     */
    protected Class<?> getParameterType(final int i) {
        if (args.length <= i) {
            throw new IllegalArgumentException("There isn't any argument at the i-th position.");
        } else if (types.length <= i) {
            return getArgumentTyp(i);
        } else {
            return types[i] == null ? getArgumentTyp(i) : types[i];
        }
    }

    /**
     * Gets the type of the argument at the {@code i}-th position.
     *
     * @param i the position the get the arguments type for
     * @return the type of the argument at the {@code i}-th position
     */
    protected Class<?> getArgumentTyp(final int i) {
        final Object arg = args[i];

        if (i == this.placeHolderIdx) {
            return IndexKeyDefinition.class;
        } else if (arg == null) {
            return Object.class;
        } else {
            return arg.getClass();
        }
    }

    /**
     * Creates the defined {@code IndexedCollection} using the specified
     * arguments and the specific {@code IndexKeyDefinition}.
     *
     * @param keyDef the {@code IndexKeyDefinition} to be used to create the
     *               instance
     * @return the new instance of the {@code IndexKeyDefinition}
     */
    public <T extends IIndexedCollection> T create(
            final IndexKeyDefinition keyDef) {

        // create the parameterlist
        final Object[] params = args.clone();
        params[this.placeHolderIdx] = keyDef;

        try {
            @SuppressWarnings("unchecked")
            final T creation = (T) this.ctor.newInstance(params);
            return creation;
        } catch (final Exception e) {
            throw new IllegalStateException(
                    "Could not construct the instance because of an error.", e);
        }
    }

    /**
     * Gets the type of the {@code IndexedCollection} created by this
     * definition.
     *
     * @return the type of the {@code IndexedCollection} created by this
     * definition
     */
    public Class<? extends IIndexedCollection> getCollectionType() {
        return type;
    }

    @Override
    public String toString() {
        return "CollectionDefinition of '" + type + "' with parameters '"
                + Arrays.asList(args).toString() + "'";
    }
}
