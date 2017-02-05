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

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;

import org.junit.Test;
import org.slf4j.LoggerFactory;

import ch.qos.logback.classic.Level;
import ch.qos.logback.classic.Logger;

/**
 * Check utility class (enumerations).
 * 
 * @since Dec 11, 2015
 * @author Gilles Landel
 * 
 */
public class EnumUtilsTest {

    /**
     * Test method for
     * {@link fr.landel.utils.commons.EnumUtils#getNullIfEmpty(java.lang.Class, java.lang.String)}
     * .
     */
    @Test
    public void testGetNullIfEmpty() {
        assertEquals(EnumUtilsData.FIELD, EnumUtils.getNullIfEmpty(EnumUtilsData.class, "FIELD"));
        assertNull(EnumUtils.getNullIfEmpty(null, "FIELD"));
        assertNull(EnumUtils.getNullIfEmpty(EnumUtilsData.class, ""));

        Logger logger = (Logger) LoggerFactory.getLogger(EnumUtils.class);
        logger.setLevel(Level.INFO);
        assertNull(EnumUtils.getNullIfEmpty(EnumUtilsData.class, "FIEL"));
        logger.setLevel(null);
        assertNull(EnumUtils.getNullIfEmpty(EnumUtilsData.class, "FIEL"));
    }
}
