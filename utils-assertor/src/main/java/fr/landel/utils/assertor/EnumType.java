/*-
 * #%L
 * utils-assertor
 * %%
 * Copyright (C) 2016 Gilandel
 * %%
 * Authors: Gilles Landel
 * URL: https://github.com/Gilandel
 * 
 * This file is under Apache License, version 2.0 (2004).
 * #L%
 */
package fr.landel.utils.assertor;

import java.util.Calendar;
import java.util.Date;
import java.util.Map;

import fr.landel.utils.commons.NumberUtils;

/**
 * List of supported type
 *
 * @since Aug 7, 2016
 * @author Gilles
 *
 */
public enum EnumType {
    /**
     * Unknown type
     */
    UNKNOWN,

    /**
     * Boolean type
     */
    BOOLEAN,

    /**
     * Number, integer type
     */
    NUMBER_INTEGER,

    /**
     * Number, floating point type
     */
    NUMBER_DECIMAL,

    /**
     * Date type
     */
    DATE,

    /**
     * String type
     */
    CHAR_SEQUENCE,

    /**
     * Class type
     */
    CLASS,

    /**
     * Iterable type
     */
    ITERABLE,

    /**
     * Map type
     */
    MAP,

    /**
     * Array type
     */
    ARRAY,

    /**
     * Calendar field (only used by converter)
     */
    CALENDAR_FIELD;

    /**
     * Get the type of an object
     * 
     * @param object
     *            the object
     * @return The type or {@link EnumType#UNKNOWN}
     */
    protected static EnumType getType(final Object object) {
        EnumType type = UNKNOWN;
        if (object != null) {
            final Class<?> clazz = object.getClass();
            if (Number.class.isAssignableFrom(clazz)) {
                if (NumberUtils.isNumberInteger(clazz)) {
                    type = NUMBER_INTEGER;
                } else {
                    type = NUMBER_DECIMAL;
                }
            } else if (CharSequence.class.isAssignableFrom(clazz)) {
                type = CHAR_SEQUENCE;
            } else if (Boolean.class.isAssignableFrom(clazz)) {
                type = BOOLEAN;
            } else if (clazz.isArray()) {
                type = ARRAY;
            } else if (Iterable.class.isAssignableFrom(clazz)) {
                type = ITERABLE;
            } else if (Map.class.isAssignableFrom(clazz)) {
                type = MAP;
            } else if (Date.class.isAssignableFrom(clazz) || Calendar.class.isAssignableFrom(clazz)) {
                type = DATE;
            } else if (Class.class.isInstance(object)) {
                type = CLASS;
            }
        }
        return type;
    }
}
