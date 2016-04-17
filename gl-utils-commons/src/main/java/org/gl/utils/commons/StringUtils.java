/*
 * #%L
 * gl-utils-commons
 * %%
 * Copyright (C) 2016 Gilandel
 * %%
 * Authors: Gilles Landel
 * URL: https://github.com/Gilandel
 * 
 * This file is under Apache License, version 2.0 (2004).
 * #L%
 */
package org.gl.utils.commons;

/**
 * Utility class to manage strings.
 *
 * @since 27 nov. 2015
 * @author Gilles Landel
 *
 */
public final class StringUtils extends org.apache.commons.lang3.StringUtils {

    /**
     * Get the char sequence if not empty and null otherwise.
     * 
     * @param cs
     *            The CharSequence to check, may be null
     * @param <C>
     *            Type of the char sequence
     * @return a char sequence or null
     */
    public static <C extends CharSequence> C getNullIfEmpty(final C cs) {
        if (isNotEmpty(cs)) {
            return cs;
        }
        return null;
    }

    /**
     * Get the char sequence if not null and not empty and the default one
     * otherwise.
     * 
     * @param cs
     *            The CharSequence to check, may be null
     * @param defaultCS
     *            The default char sequence
     * @param <C>
     *            Type of the char sequence
     * @return a char sequence
     */
    public static <C extends CharSequence> C getDefaultIfEmpty(final C cs, final C defaultCS) {
        if (isNotEmpty(cs)) {
            return cs;
        }
        return defaultCS;
    }

    /**
     * Get the char sequence if not null and the default one otherwise.
     * 
     * @param cs
     *            The CharSequence to check, may be null
     * @param defaultCS
     *            The default char sequence
     * @param <C>
     *            Type of the char sequence
     * @return a char sequence
     */
    public static <C extends CharSequence> C getDefaultIfNull(final C cs, final C defaultCS) {
        if (cs != null) {
            return cs;
        }
        return defaultCS;
    }

    /**
     * Get the toString if not null and the default one otherwise.
     * 
     * @param obj
     *            The object to check, may be null
     * @param defaultStr
     *            The default string
     * @return a string
     */
    public static String getToStringOrDefaultIfNull(final Object obj, final String defaultStr) {
        if (obj != null) {
            return obj.toString();
        }
        return defaultStr;
    }
}
