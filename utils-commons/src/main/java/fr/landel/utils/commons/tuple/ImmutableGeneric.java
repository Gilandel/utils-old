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
 * An immutable generic consisting of {@code Object} elements.
 * </p>
 * 
 * <p>
 * Although the implementation is immutable, there is no restriction on the
 * objects that may be stored. If mutable objects are stored in the generic,
 * then the generic itself effectively becomes mutable. The class is also
 * {@code final}, so a subclass can not add undesirable behaviour.
 * </p>
 * 
 * <p>
 * #ThreadSafe# if all objects are thread-safe
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
public final class ImmutableGeneric<T> extends AbstractImmutableGeneric<T> {

    /**
     * serialVersionUID
     */
    private static final long serialVersionUID = -8620414093268112166L;

    /**
     * Create a new immutable generic instance.
     *
     * @param objects
     *            the objects, may be null but not the whole array, ex:
     *            <tt>(Object[]) null</tt>
     */
    @SafeVarargs
    public ImmutableGeneric(final T... objects) {
        super(objects);
    }
}
