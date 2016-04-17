/*
 * #%L
 * gl-utils-commons
 * %%
 * Copyright (C) 2016 Gilandel
 * %%
 * Authors: Gilles Landel
 * 
 * This code is under Apache License, version 2.0 (2004).
 * #L%
 */
package org.gl.utils.commons;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

import java.util.Arrays;

import org.junit.Test;

/**
 * Check hexadecimal utils
 *
 * @since 11 déc. 2015
 * @author Gilles Landel
 *
 */
public class HexUtilsTest {

    /**
     * 
     * Constructor
     *
     */
    public HexUtilsTest() {
    }

    /**
     * Test method for
     * {@link org.gl.utils.commons.HexUtils#intToByte(int)}
     * .
     */
    @Test
    public void testIntToByte() {
        final int expected = 40;
        final int input = 32;

        assertEquals((byte) expected, HexUtils.intToByte(input));
    }

    /**
     * Test method for
     * {@link org.gl.utils.commons.HexUtils#intToBytes(int)}
     * .
     */
    @Test
    public void testIntToBytes() {
        final byte[] expected = {-128, 18, -20, 74};
        final int input = 1256985216;

        assertTrue(Arrays.equals(expected, HexUtils.intToBytes(input)));
    }

    /**
     * Test method for
     * {@link org.gl.utils.commons.HexUtils#byteArrayToHexString(byte[])}
     * .
     */
    @Test
    public void testByteArrayToHexString() {
        byte[] bytes = {'A', 'B', 'C'};

        assertEquals("414243", HexUtils.byteArrayToHexString(bytes));
    }
}
