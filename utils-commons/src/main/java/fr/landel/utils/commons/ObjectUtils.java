/*-
8 * #%L
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
package fr.landel.utils.commons;

import java.util.Objects;
import java.util.function.Function;
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
     * Returns a default value if the object passed is {@code null}.
     * 
     * <pre>
     * ObjectUtils.defaultIfNull(null, null, o -&gt; o.getTitle())              = null
     * ObjectUtils.defaultIfNull(null, "",  o -&gt; o.getTitle())               = ""
     * ObjectUtils.defaultIfNull(null, "zz", o -&gt; o.getTitle())              = "zz"
     * ObjectUtils.defaultIfNull("abc", "NO_TEXT", o -&gt; o.toUpperCase())     = "ABC"
     * ObjectUtils.defaultIfNull("false", Boolean.TRUE, Boolean::parseBoolean)  = Boolean.TRUE
     * </pre>
     * 
     * @param object
     *            the object to check
     * @param defaultObject
     *            the default value
     * @param transformer
     *            the object mapper
     * @param <T>
     *            the type of the object to check
     * @param <X>
     *            the type of the output
     * @return transformed {@code object} if not {@code null}, default value
     *         otherwise
     * @throws NullPointerException
     *             if {@code transformer} is {@code null}
     */
    public static <T, X> X defaultIfNull(final T object, final X defaultObject, final Function<T, X> transformer) {
        Objects.requireNonNull(transformer, "The parameter transformer cannot be null");

        return object != null ? transformer.apply(object) : defaultObject;
    }

    /**
     * Returns a default value if the object passed is {@code null}.
     *
     * <pre>
     * ObjectUtils.defaultIfNull(null, () -&gt; null)          = null
     * ObjectUtils.defaultIfNull(null, () -&gt; "")            = ""
     * ObjectUtils.defaultIfNull(null, () -&gt; "zz")          = "zz"
     * ObjectUtils.defaultIfNull("abc", () -&gt; *)            = "abc"
     * ObjectUtils.defaultIfNull(Boolean.TRUE, () -&gt;  *)    = Boolean.TRUE
     * </pre>
     *
     * @param object
     *            the {@code Object} to test, may be {@code null}
     * @param defaultValueSupplier
     *            the default value supplier, cannot be {@code null}, may supply
     *            {@code null}
     * @param <T>
     *            the type of the object
     * @return {@code object} if it is not {@code null}, defaultValue otherwise
     * @throws NullPointerException
     *             if supplier is {@code null}
     */
    public static <T> T defaultIfNull(final T object, final Supplier<? extends T> defaultValueSupplier) {
        Objects.requireNonNull(defaultValueSupplier, "The parameter defaultValueSupplier cannot be null");

        return object != null ? object : defaultValueSupplier.get();
    }

    /**
     * Check if all objects are {@code null}.
     * 
     * <p>
     * precondition: {@code objects} cannot be {@code null}
     * </p>
     * 
     * <pre>
     * ObjectUtils.allNull(5, true); // =&gt; false
     * ObjectUtils.allNull(5, null); // =&gt; false
     * ObjectUtils.allNull(null, null); // =&gt; true
     * ObjectUtils.allNull((Object) null); // =&gt; true
     * 
     * ObjectUtils.allNull((Object[]) null); // =&gt; throw a NullPointerException
     * </pre>
     * 
     * @param objects
     *            The list of objects to check
     * @return true, if all are {@code null}
     */
    public static boolean allNull(final Object... objects) {
        Objects.requireNonNull(objects, "The parameter objects cannot be null");

        boolean notNull = false;
        for (Object object : objects) {
            notNull |= object != null;
        }
        return !notNull;
    }

    /**
     * Check if any objects are {@code null}.
     * 
     * <p>
     * precondition: {@code objects} cannot be {@code null}
     * </p>
     * 
     * <pre>
     * ObjectUtils.anyNull(5, true); // =&gt; false
     * ObjectUtils.anyNull(5, null); // =&gt; true
     * ObjectUtils.anyNull(null, null); // =&gt; true
     * ObjectUtils.anyNull((Object) null); // =&gt; true
     * 
     * ObjectUtils.anyNull((Object[]) null); // =&gt; throw a NullPointerException
     * </pre>
     * 
     * @param objects
     *            The list of objects to check
     * @return true, if any are {@code null}
     */
    public static boolean anyNull(final Object... objects) {
        Objects.requireNonNull(objects, "The parameter objects cannot be null");

        for (Object object : objects) {
            if (object == null) {
                return true;
            }
        }
        return false;
    }

    /**
     * Check if all objects are not {@code null}.
     * 
     * <p>
     * precondition: {@code objects} cannot be {@code null}
     * </p>
     * 
     * <pre>
     * ObjectUtils.allNotNull(5, true); // =&gt; true
     * ObjectUtils.allNotNull(5, null); // =&gt; false
     * ObjectUtils.allNotNull(null, null); // =&gt; false
     * ObjectUtils.allNotNull((Object) null); // =&gt; false
     * 
     * ObjectUtils.allNotNull((Object[]) null); // =&gt; throw a
     *                                          // NullPointerException
     * </pre>
     * 
     * @param objects
     *            The list of objects to check
     * @return true, if all are {@code null}
     */
    public static boolean allNotNull(final Object... objects) {
        Objects.requireNonNull(objects, "The parameter objects cannot be null");

        boolean areNull = false;
        for (Object object : objects) {
            areNull |= object == null;
        }
        return !areNull;
    }

    /**
     * Check if any objects are not{@code null}.
     * 
     * <p>
     * precondition: {@code objects} cannot be {@code null}
     * </p>
     * 
     * <pre>
     * ObjectUtils.anyNotNull(5, true); // =&gt; true
     * ObjectUtils.anyNotNull(5, null); // =&gt; true
     * ObjectUtils.anyNotNull(null, null); // =&gt; false
     * ObjectUtils.anyNotNull((Object) null); // =&gt; false
     * 
     * ObjectUtils.anyNotNull((Object[]) null); // =&gt; throw a
     *                                          // NullPointerException
     * </pre>
     * 
     * @param objects
     *            The list of objects to check
     * @return true, if any are {@code null}
     */
    public static boolean anyNotNull(final Object... objects) {
        Objects.requireNonNull(objects, "The parameter objects cannot be null");

        for (Object object : objects) {
            if (object != null) {
                return true;
            }
        }
        return false;
    }
}
