/*-
 * #%L
 * utils-commons
 * %%
 * Copyright (C) 2016 - 2017 Gilandel
 * %%
 * Authors: Gilles Landel
 * URL: https://github.com/Gilandel
 * 
 * This file is under Apache License, version 2.0 (2004).
 * #L%
 */
package fr.landel.utils.commons.tuple;

/**
 * 
 * <p>
 * An immutable pair consisting of two {@code Object} elements.
 * </p>
 * 
 * <p>
 * Although the implementation is immutable, there is no restriction on the
 * objects that may be stored. If mutable objects are stored in the pair, then
 * the pair itself effectively becomes mutable. The class is also {@code final},
 * so a subclass can not add undesirable behaviour.
 * </p>
 * 
 * <p>
 * #ThreadSafe# if both paired objects are thread-safe
 * </p>
 *
 * @param <T>
 *            the element type
 * 
 * @see org.apache.commons.lang3.tuple.Pair
 *
 * @since Jul 26, 2016
 * @author Gilles
 *
 */
public final class ImmutablePairIso<T> extends AbstractImmutablePairIso<T> {

    /**
     * serialVersionUID
     */
    private static final long serialVersionUID = -6426023549369828124L;

    /**
     * Create a new immutable pair instance.
     *
     * @param left
     *            the left value, may be null
     * @param right
     *            the right value, may be null
     */
    public ImmutablePairIso(final T left, final T right) {
        super(left, right);
    }
}
