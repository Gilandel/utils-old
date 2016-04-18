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

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;

import org.junit.Test;

/**
 * Check utility class (enumerations).
 * 
 * @since 11 dec. 2015
 * @author Gilles Landel
 * 
 */
public class EnumUtilsTest {

    /**
     * Test method for
     * {@link org.gl.utils.commons.EnumUtils#getNullIfEmpty(java.lang.Class, java.lang.String)}
     * .
     */
    @Test
    public void testGetNullIfEmpty() {
        assertEquals(EnumUtilsData.FIELD, EnumUtils.getNullIfEmpty(EnumUtilsData.class, "FIELD"));
        assertNull(EnumUtils.getNullIfEmpty(null, "FIELD"));
        assertNull(EnumUtils.getNullIfEmpty(EnumUtilsData.class, "FIEL"));
    }
}
