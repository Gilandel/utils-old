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
 * A quad consisting of four elements.
 * </p>
 * 
 * <p>
 * This class is an abstract implementation defining the basic API. It refers to
 * the elements as 'first', 'second', 'third' and 'fourth'.
 * </p>
 * 
 * <p>
 * Subclass implementations may be mutable or immutable. However, there is no
 * restriction on the type of the stored objects that may be stored. If mutable
 * objects are stored in the quad, then the quad itself effectively becomes
 * mutable.
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
public abstract class Quad<A, B, C, D> extends AbstractQuad<A, B, C, D> {

    /**
     * serialVersionUID
     */
    private static final long serialVersionUID = -7359128872551227845L;

    /**
     * <p>
     * Obtains an immutable quad of from four objects inferring the generic
     * types.
     * </p>
     * 
     * <p>
     * This factory allows the quad to be created using inference to obtain the
     * generic types.
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
     * @param first
     *            the first element, may be null
     * @param second
     *            the second element, may be null
     * @param third
     *            the third element, may be null
     * @param fourth
     *            the fourth element, may be null
     * @return a quad formed from the four parameters, not null
     */
    public static <A, B, C, D> Quad<A, B, C, D> of(final A first, final B second, final C third, final D fourth) {
        return new ImmutableQuad<A, B, C, D>(first, second, third, fourth);
    }

    /**
     * <p>
     * Obtains a mutable quad of from four objects inferring the generic types.
     * </p>
     * 
     * <p>
     * This factory allows the quad to be created using inference to obtain the
     * generic types.
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
     * @param first
     *            the first element, may be null
     * @param second
     *            the second element, may be null
     * @param third
     *            the third element, may be null
     * @param fourth
     *            the fourth element, may be null
     * @return a quad formed from the four parameters, not null
     */
    public static <A, B, C, D> MutableQuad<A, B, C, D> ofMutable(final A first, final B second, final C third, final D fourth) {
        return new MutableQuad<A, B, C, D>(first, second, third, fourth);
    }
}
