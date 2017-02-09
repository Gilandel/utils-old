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
 * An immutable single consisting of a single {@code Object} element.
 * </p>
 * 
 * <p>
 * Although the implementation is immutable, there is no restriction on the
 * objects that may be stored. If mutable objects are stored in the single, then
 * the single itself effectively becomes mutable.
 * </p>
 * 
 * <p>
 * #ThreadSafe# if the object is thread-safe
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
public abstract class AbstractImmutableSingle<T> extends Single<T> {

    /**
     * serialVersionUID
     */
    private static final long serialVersionUID = 1867548171293331171L;

    /** object */
    public final T element;

    /**
     * Create a new immutable single instance.
     *
     * @param element
     *            the value, may be null
     */
    public AbstractImmutableSingle(final T element) {
        super();
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
     * <p>
     * Throws {@code UnsupportedOperationException}.
     * </p>
     * 
     * <p>
     * This single is immutable, so this operation is not supported.
     * </p>
     *
     * @param element
     *            the value to set
     * @return never
     * @throws UnsupportedOperationException
     *             as this operation is not supported
     */
    @Override
    public T set(final T element) {
        throw new UnsupportedOperationException();
    }
}
