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
 * @param <A>
 *            the type of first object
 * @param <B>
 *            the type of second object
 * @param <C>
 *            the type of third object
 * @param <D>
 *            the type of fourth object
 */
public class MutableQuad<A, B, C, D> extends Quad<A, B, C, D> {

    /**
     * serialVersionUID
     */
    private static final long serialVersionUID = -799774348810900228L;

    /** First object */
    private A first;
    /** Second object */
    private B second;
    /** Third object */
    private C third;
    /** Fourth object */
    private D fourth;

    /**
     * Create a new triple instance of three nulls.
     */
    public MutableQuad() {
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
    public MutableQuad(final A first, final B second, final C third, final D fourth) {
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
    public A getFirst() {
        return this.first;
    }

    /**
     * Sets the first element of the quad.
     * 
     * @param first
     *            the new value of the first element, may be null
     */
    public void setFirst(final A first) {
        this.first = first;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public B getSecond() {
        return this.second;
    }

    /**
     * Sets the second element of the quad.
     * 
     * @param second
     *            the new value of the second element, may be null
     */
    public void setSecond(final B second) {
        this.second = second;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public C getThird() {
        return this.third;
    }

    /**
     * Sets the third element of the quad.
     * 
     * @param third
     *            the new value of the third element, may be null
     */
    public void setThird(final C third) {
        this.third = third;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public D getFourth() {
        return this.fourth;
    }

    /**
     * Sets the fourth element of the quad.
     * 
     * @param fourth
     *            the new value of the fourth element, may be null
     */
    public void setFourth(final D fourth) {
        this.fourth = fourth;
    }
}
