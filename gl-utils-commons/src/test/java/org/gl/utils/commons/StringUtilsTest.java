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
 * Check utility class (strings).
 *
 * @since 27 nov. 2015
 * @author Gilles Landel
 *
 */
public class StringUtilsTest {

    /**
     * Test method for
     * {@link org.gl.utils.commons.StringUtils#getNullIfEmpty(java.lang.CharSequence)}
     * .
     */
    @Test
    public void testGetNullIfEmpty() {
        String expected = "value";
        assertEquals(expected, StringUtils.getNullIfEmpty(expected));

        assertNull(StringUtils.getNullIfEmpty(""));
        assertNull(StringUtils.getNullIfEmpty(null));
    }

    /**
     * Test method for
     * {@link org.gl.utils.commons.StringUtils#getDefaultIfEmpty(java.lang.CharSequence, java.lang.CharSequence)}
     * .
     */
    @Test
    public void testGetDefaultIfEmpty() {
        String expected = "value";
        String defaultValue = "def";
        assertEquals(expected, StringUtils.getDefaultIfEmpty(expected, defaultValue));

        assertEquals(defaultValue, StringUtils.getDefaultIfEmpty("", defaultValue));
        assertEquals(defaultValue, StringUtils.getDefaultIfEmpty(null, defaultValue));
    }

    /**
     * Test method for
     * {@link org.gl.utils.commons.StringUtils#getDefaultIfNull(java.lang.CharSequence, java.lang.CharSequence)}
     * .
     */
    @Test
    public void testGetDefaultIfNull() {
        String expected = "value";
        String defaultValue = "def";
        assertEquals(expected, StringUtils.getDefaultIfNull(expected, defaultValue));

        assertEquals("", StringUtils.getDefaultIfNull("", defaultValue));
        assertEquals(defaultValue, StringUtils.getDefaultIfNull(null, defaultValue));
    }

    /**
     * Test method for
     * {@link org.gl.utils.commons.StringUtils#getToStringOrDefaultIfNull(java.lang.Object, java.lang.String)}
     * .
     */
    @Test
    public void testGetToStringOrDefaultIfNull() {
        Long expected = 1L;
        String defaultValue = "def";
        assertEquals(String.valueOf(expected), StringUtils.getToStringOrDefaultIfNull(expected, defaultValue));

        assertEquals("", StringUtils.getToStringOrDefaultIfNull("", defaultValue));
        assertEquals(defaultValue, StringUtils.getToStringOrDefaultIfNull(null, defaultValue));
    }
}
