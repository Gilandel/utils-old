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
 * A mutable pair consisting of two {@code Object} elements.
 * </p>
 * 
 * <p>
 * Not #ThreadSafe#
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
public class MutablePairIso<T> extends PairIso<T> {

    /**
     * serialVersionUID
     */
    private static final long serialVersionUID = -6102457045775716086L;

    /** Left object */
    public T left;
    /** Right object */
    public T right;

    /**
     * Create a new pair instance of two nulls.
     */
    public MutablePairIso() {
        super();
    }

    /**
     * Create a new mutable pair instance.
     *
     * @param left
     *            the left value, may be null
     * @param right
     *            the right value, may be null
     */
    public MutablePairIso(final T left, final T right) {
        this();
        this.left = left;
        this.right = right;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public T getLeft() {
        return left;
    }

    /**
     * Sets the left element of the pair.
     * 
     * @param left
     *            the new value of the left element, may be null
     */
    public void setLeft(final T left) {
        this.left = left;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public T getRight() {
        return right;
    }

    /**
     * Sets the right element of the pair.
     * 
     * @param right
     *            the new value of the right element, may be null
     */
    public void setRight(final T right) {
        this.right = right;
    }

    /**
     * Sets the {@code Map.Entry} value. This sets the right element of the
     * pair.
     * 
     * @param value
     *            the right value to set, not null
     * @return the old value for the right element
     */
    @Override
    public T setValue(final T value) {
        final T result = getRight();
        setRight(value);
        return result;
    }
}
