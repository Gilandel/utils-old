/*
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
package fr.landel.utils.commons;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * Utility class to manage enumerations.
 *
 * @since Nov 27, 2015
 * @author Gilles Landel
 *
 */
public final class EnumUtils extends org.apache.commons.lang3.EnumUtils {

    private static final Logger LOGGER = LoggerFactory.getLogger(EnumUtils.class);

    /**
     * Hidden constructor.
     */
    private EnumUtils() {
        super();
    }

    /**
     * Get the enumeration if name is not empty and null otherwise.
     * 
     * @param enumType
     *            The type of the enumeration
     * @param name
     *            The string to check, may be null
     * @param <T>
     *            Type of the enumeration
     * @return The enumeration object or null
     */
    public static <T extends Enum<T>> T getNullIfEmpty(final Class<T> enumType, final String name) {
        if (StringUtils.isNotEmpty(name) && enumType != null) {
            try {
                return Enum.valueOf(enumType, name);
            } catch (IllegalArgumentException e) {
                if (LOGGER.isDebugEnabled()) {
                    LOGGER.debug("Name parameter '" + name + "' not found in enumeration: " + enumType, e);
                }
            }
        }
        return null;
    }
}
