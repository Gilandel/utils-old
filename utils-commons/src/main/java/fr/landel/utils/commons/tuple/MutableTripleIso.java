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
 * A mutable triple consisting of three {@code Object} elements.
 * </p>
 * 
 * <p>
 * Not #ThreadSafe#
 * </p>
 *
 * @param <T>
 *            the element type
 * 
 * @see org.apache.commons.lang3.tuple.Triple
 *
 * @since Jul 26, 2016
 * @author Gilles
 *
 */
public class MutableTripleIso<T> extends TripleIso<T> {

    /**
     * serialVersionUID
     */
    private static final long serialVersionUID = -799774348810900228L;

    /** Left object */
    private T left;
    /** Middle object; */
    private T middle;
    /** Right object */
    private T right;

    /**
     * Create a new mutable triple instance of three nulls.
     */
    public MutableTripleIso() {
        super();
    }

    /**
     * Create a new triple instance.
     *
     * @param left
     *            the left value, may be null
     * @param middle
     *            the middle value, may be null
     * @param right
     *            the right value, may be null
     */
    public MutableTripleIso(final T left, final T middle, final T right) {
        this();
        this.left = left;
        this.middle = middle;
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
     * Sets the left element of the triple.
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
    public T getMiddle() {
        return middle;
    }

    /**
     * Sets the middle element of the triple.
     * 
     * @param middle
     *            the new value of the middle element, may be null
     */
    public void setMiddle(final T middle) {
        this.middle = middle;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public T getRight() {
        return right;
    }

    /**
     * Sets the right element of the triple.
     * 
     * @param right
     *            the new value of the right element, may be null
     */
    public void setRight(final T right) {
        this.right = right;
    }
}
