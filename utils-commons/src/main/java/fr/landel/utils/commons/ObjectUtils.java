/*-
 * #%L
 * utils-commons
 * %%
 * Copyright (C) 2016 Gilandel
 * %%
 * Authors: Gilles Landel
 * URL: https://github.com/Gilandel
 * 
 * This file is under Apache License, version 2.0 (2004).
 * #L%
 */
package fr.landel.utils.commons;

import java.util.Objects;
import java.util.function.Supplier;

/**
 * <p>
 * Operations on {@code Object}.
 * </p>
 *
 * <p>
 * This class tries to handle {@code null} input gracefully. An exception will
 * generally not be thrown for a {@code null} input. Each method documents its
 * behaviour in more detail.
 * </p>
 *
 * <p>
 * #ThreadSafe#
 * </p>
 *
 * @since Aug 15, 2016
 * @author Gilles
 *
 */
public class ObjectUtils extends org.apache.commons.lang3.ObjectUtils {

    /**
     * {@inheritDoc}
     */
    public ObjectUtils() {
        super();
    }

    /**
     * <p>
     * Returns a default value if the object passed is {@code null}.
     * </p>
     *
     * <pre>
     * ObjectUtils.defaultIfNull(null, null)      = null
     * ObjectUtils.defaultIfNull(null, "")        = ""
     * ObjectUtils.defaultIfNull(null, "zz")      = "zz"
     * ObjectUtils.defaultIfNull("abc", *)        = "abc"
     * ObjectUtils.defaultIfNull(Boolean.TRUE, *) = Boolean.TRUE
     * </pre>
     *
     * @param <T>
     *            the type of the object
     * @param object
     *            the {@code Object} to test, may be {@code null}
     * @param defaultValueSupplier
     *            the default value supplier, cannot be {@code null}, may supply
     *            {@code null}
     * @return {@code object} if it is not {@code null}, defaultValue otherwise
     */
    public static <T> T defaultIfNull(final T object, final Supplier<T> defaultValueSupplier) {
        Objects.requireNonNull(defaultValueSupplier);

        return object != null ? object : defaultValueSupplier.get();
    }
}