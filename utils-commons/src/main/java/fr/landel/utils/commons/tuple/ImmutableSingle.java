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
 * An immutable single consisting of a single {@code Object} element.
 * </p>
 * 
 * <p>
 * Although the implementation is immutable, there is no restriction on the
 * objects that may be stored. If mutable objects are stored in the single, then
 * the single itself effectively becomes mutable. The class is also
 * {@code final}, so a subclass can not add undesirable behaviour.
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
public final class ImmutableSingle<T> extends AbstractImmutableSingle<T> {

    /**
     * serialVersionUID
     */
    private static final long serialVersionUID = -6426023549359828124L;

    /**
     * Create a new immutable single instance.
     *
     * @param element
     *            the value, may be null
     */
    public ImmutableSingle(final T element) {
        super(element);
    }
}
