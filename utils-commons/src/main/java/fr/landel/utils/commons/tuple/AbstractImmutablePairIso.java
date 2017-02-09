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
 * <p>
 * Abstract version for overriding purpose, don't forgot to keep it immutable.
 * </p>
 * 
 * <p>
 * An immutable pair consisting of two {@code Object} elements.
 * </p>
 * 
 * <p>
 * Although the implementation is immutable, there is no restriction on the
 * objects that may be stored. If mutable objects are stored in the pair, then
 * the pair itself effectively becomes mutable.
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
public abstract class AbstractImmutablePairIso<T> extends PairIso<T> {

    /**
     * serialVersionUID
     */
    private static final long serialVersionUID = 1867548171293331171L;

    /** Left object */
    public final T left;
    /** Right object */
    public final T right;

    /**
     * Create a new immutable pair instance.
     *
     * @param left
     *            the left value, may be null
     * @param right
     *            the right value, may be null
     */
    public AbstractImmutablePairIso(final T left, final T right) {
        super();
        this.left = left;
        this.right = right;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public T getLeft() {
        return this.left;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public T getRight() {
        return this.right;
    }

    /**
     * <p>
     * Throws {@code UnsupportedOperationException}.
     * </p>
     * 
     * <p>
     * This pair is immutable, so this operation is not supported.
     * </p>
     *
     * @param value
     *            the value to set
     * @return never
     * @throws UnsupportedOperationException
     *             as this operation is not supported
     */
    @Override
    public T setValue(final T value) {
        throw new UnsupportedOperationException();
    }
}
