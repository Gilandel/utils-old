/*
 * #%L
 * utils-model
 * %%
 * Copyright (C) 2016 - 2017 Gilandel
 * %%
 * Authors: Gilles Landel
 * URL: https://github.com/Gilandel
 * 
 * This file is under Apache License, version 2.0 (2004).
 * #L%
 */
package fr.landel.utils.model.mappable;

import java.io.Serializable;
import java.util.Collections;
import java.util.HashMap;
import java.util.Map;

import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonValue;

/**
 * Enumeration of supported locales.
 *
 * @since Nov 27, 2015
 * @author Gilles
 *
 */
public enum EnumLocale implements Serializable {

    /**
     * The french locale
     */
    fr,

    /**
     * The english locale
     */
    en;

    /**
     * The reversed enumeration map (key: name, value:
     * {@link fr.landel.utils.model.mappable.EnumLocale})
     */
    public static final Map<String, EnumLocale> ENUM_REVERSE_VALUE;

    static {
        Map<String, EnumLocale> map = new HashMap<>();
        for (EnumLocale value : EnumLocale.values()) {
            map.put(value.getName(), value);
        }
        ENUM_REVERSE_VALUE = Collections.unmodifiableMap(map);
    }

    @JsonValue
    public String getName() {
        return this.name();
    }

    /**
     * Deserializes json into java enum
     * 
     * @param val
     *            The JSON value
     * @return The Enum
     */
    @JsonCreator
    public static EnumLocale create(final String val) {
        return ENUM_REVERSE_VALUE.get(val);
    }
}
