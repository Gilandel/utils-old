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
 * A mutable single consisting of a single {@code Object} element.
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
public class MutableSingle<T> extends Single<T> {

    /**
     * serialVersionUID
     */
    private static final long serialVersionUID = -6102457045775716036L;

    /** object */
    public T element;

    /**
     * Create a new single instance of one null.
     */
    public MutableSingle() {
        super();
    }

    /**
     * Create a new mutable single instance.
     *
     * @param element
     *            the value, may be null
     */
    public MutableSingle(final T element) {
        this();
        this.element = element;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public T get() {
        return this.element;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public T set(final T element) {
        T previous = this.element;
        this.element = element;
        return previous;
    }
}
