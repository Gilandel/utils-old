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
 * An immutable quad consisting of four {@code Object} elements of same type.
 * </p>
 * 
 * <p>
 * Although the implementation is immutable, there is no restriction on the
 * objects that may be stored. If mutable objects are stored in the quad, then
 * the quad itself effectively becomes mutable. The class is also {@code final},
 * so a subclass can not add undesirable behaviour.
 * </p>
 * 
 * <p>
 * #ThreadSafe# if both quad objects are thread-safe
 * </p>
 *
 * @param <T>
 *            the element type
 *
 * @since Jul 26, 2016
 * @author Gilles
 *
 */
public final class ImmutableQuadIso<T> extends AbstractImmutableQuadIso<T> {

    /**
     * serialVersionUID
     */
    private static final long serialVersionUID = -1921573496708644028L;

    /**
     * Create a new immutable quad instance.
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
    public ImmutableQuadIso(final T first, final T second, final T third, final T fourth) {
        super(first, second, third, fourth);
    }
}
