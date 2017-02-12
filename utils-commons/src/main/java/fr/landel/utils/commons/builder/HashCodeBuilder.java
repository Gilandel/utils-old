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
package fr.landel.utils.commons.builder;

import java.util.Objects;
import java.util.function.Function;

/**
 * {@link org.apache.commons.lang3.builder.HashCodeBuilder}
 * 
 * @since Nov 15, 2016
 * @author Gilles
 *
 */
public class HashCodeBuilder extends org.apache.commons.lang3.builder.HashCodeBuilder {

    /**
     * Append the {@code hashCode} returned by the {@code getter} function. The
     * {@code getter} method is only applied if the {@link Object} are not
     * {@code null}.
     * 
     * @param object
     *            the first object
     * @param getter
     *            the function to apply if both objects are not {@code null}
     *            (required, throws a {@link NullPointerException} if
     *            {@code null})
     * @param <T>
     *            the check object type
     * @param <X>
     *            the sub type
     * @return the current builder
     */
    public <T, X> HashCodeBuilder append(final T object, final Function<T, X> getter) {
        Objects.requireNonNull(getter);
        if (object != null) {
            this.append(getter.apply(object));
        }
        return this;
    }
}
