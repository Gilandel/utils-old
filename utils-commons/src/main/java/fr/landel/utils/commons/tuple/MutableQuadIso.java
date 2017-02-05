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
 * A mutable quad consisting of four {@code Object} elements.
 * </p>
 * 
 * <p>
 * Not #ThreadSafe#
 * </p>
 *
 * @see org.apache.commons.lang3.tuple.Triple
 *
 * @since Jul 26, 2016
 * @author Gilles
 *
 * @param <T>
 *            the type of objects
 */
public class MutableQuadIso<T> extends QuadIso<T> {

    /**
     * serialVersionUID
     */
    private static final long serialVersionUID = -799774348810900228L;

    /** First object */
    private T first;
    /** Second object */
    private T second;
    /** Third object */
    private T third;
    /** Fourth object */
    private T fourth;

    /**
     * Create a new triple instance of three nulls.
     */
    public MutableQuadIso() {
        super();
    }

    /**
     * Create a new mutable quad instance.
     *
     * @param first
     *            the first element, may be null
     * @param second
     *            the second element, may be null
     * @param third
     *            the third element, may be null
     * @param fourth
     *            the fourth element, may be null
     */
    public MutableQuadIso(final T first, final T second, final T third, final T fourth) {
        this();
        this.first = first;
        this.second = second;
        this.third = third;
        this.fourth = fourth;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public T getFirst() {
        return this.first;
    }

    /**
     * Sets the first element of the quad.
     * 
     * @param first
     *            the new value of the first element, may be null
     */
    public void setFirst(final T first) {
        this.first = first;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public T getSecond() {
        return this.second;
    }

    /**
     * Sets the second element of the quad.
     * 
     * @param second
     *            the new value of the second element, may be null
     */
    public void setSecond(final T second) {
        this.second = second;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public T getThird() {
        return this.third;
    }

    /**
     * Sets the third element of the quad.
     * 
     * @param third
     *            the new value of the third element, may be null
     */
    public void setThird(final T third) {
        this.third = third;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public T getFourth() {
        return this.fourth;
    }

    /**
     * Sets the fourth element of the quad.
     * 
     * @param fourth
     *            the new value of the fourth element, may be null
     */
    public void setFourth(final T fourth) {
        this.fourth = fourth;
    }
}
