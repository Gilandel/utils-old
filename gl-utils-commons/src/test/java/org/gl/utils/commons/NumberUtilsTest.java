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
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;

import org.junit.Test;

/**
 * Check utility class (numbers).
 *
 * @since 27 nov. 2015
 * @author Gilles Landel
 *
 */
public class NumberUtilsTest {

    /**
     * Test method for {@link NumberUtils#isEqual(Double, Double)} .
     */
    @Test
    public void testIsEqualDoubleDouble() {
        assertTrue(NumberUtils.isEqual(5565680.25d, 5565680.25d));
        assertTrue(NumberUtils.isEqual(0.00000000000000000000000001d, 0.00000000000000000000000001d));
        assertFalse(NumberUtils.isEqual(0.00000000000000000000000001d, 0.00000000000000000000000002d));
        assertFalse(NumberUtils.isEqual(0.0000000000000000000000001d, 0.0000000000000000000000002d));
    }

    /**
     * Test method for {@link NumberUtils#isEqual(Double, Double, Integer)} .
     */
    @Test
    public void testIsEqualDoubleDoubleInt() {
        assertTrue(NumberUtils.isEqual(0.25d, 0.25d, 5));
        assertTrue(NumberUtils.isEqual(0.00012d, 0.00013d, 5));
        assertFalse(NumberUtils.isEqual(0.00012d, 0.00013d, 6));
    }

    /**
     * Test method for {@link NumberUtils#isEqual(Float, Float)} .
     */
    @Test
    public void testIsEqualFloatFloat() {
        assertTrue(NumberUtils.isEqual(5565680.25f, 5565680.25f));
        assertTrue(NumberUtils.isEqual(0.0000000000000000000000001f, 0.0000000000000000000000001f));
        assertFalse(NumberUtils.isEqual(0.00000000000000000000000001f, 0.00000000000000000000000002f));
        assertFalse(NumberUtils.isEqual(0.0000000000000000000000001f, 0.0000000000000000000000002f));
    }

    /**
     * Test method for {@link NumberUtils#isEqual(Float, Float, Integer)} .
     */
    @Test
    public void testIsEqualFloatFloatInt() {
        assertTrue(NumberUtils.isEqual(0.25f, 0.25f, 5));
        assertTrue(NumberUtils.isEqual(0.000012f, 0.000013f, 5));
        assertFalse(NumberUtils.isEqual(0.00012f, 0.00013f, 6));
    }

    /**
     * Test method for {@link NumberUtils#getDefaultIfNull(Number, Number)} .
     */
    @Test
    public void testGetDefaultIfNull() {
        final Float num = 10.0f;
        final Float defaultNum = 15.0f;

        assertEquals(num, NumberUtils.getDefaultIfNull(num, defaultNum));
        assertEquals(defaultNum, NumberUtils.getDefaultIfNull(null, defaultNum));
        assertEquals(num, NumberUtils.getDefaultIfNull(num, null));
        assertNull(NumberUtils.getDefaultIfNull(null, null));
    }
}
