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
 * An immutable quad consisting of four {@code Object} elements.
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
 * @param <A>
 *            the first element type
 * @param <B>
 *            the second element type
 * @param <C>
 *            the third element type
 * @param <D>
 *            the fourth element type
 *
 * @since Jul 26, 2016
 * @author Gilles
 *
 */
public final class ImmutableQuad<A, B, C, D> extends AbstractImmutableQuad<A, B, C, D> {

    /**
     * serialVersionUID
     */
    private static final long serialVersionUID = -2206706642422589043L;

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
    public ImmutableQuad(final A first, final B second, final C third, final D fourth) {
        super(first, second, third, fourth);
    }
}
