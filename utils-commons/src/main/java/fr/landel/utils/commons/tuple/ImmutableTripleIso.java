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
 * An immutable triple consisting of three {@code Object} elements.
 * </p>
 * 
 * <p>
 * Although the implementation is immutable, there is no restriction on the
 * objects that may be stored. If mutable objects are stored in the triple, then
 * the triple itself effectively becomes mutable. The class is also
 * {@code final}, so a subclass can not add undesirable behaviour.
 * </p>
 * 
 * <p>
 * #ThreadSafe# if both tripled objects are thread-safe
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
public final class ImmutableTripleIso<T> extends AbstractImmutableTripleIso<T> {

    /**
     * serialVersionUID
     */
    private static final long serialVersionUID = -4594913866105614916L;

    /**
     * Create a new immutable triple instance.
     *
     * @param left
     *            the left value, may be null
     * @param middle
     *            the middle value, may be null
     * @param right
     *            the right value, may be null
     */
    public ImmutableTripleIso(final T left, final T middle, final T right) {
        super(left, middle, right);
    }
}
