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
import static org.junit.Assert.assertTrue;

import java.util.Arrays;

import org.junit.Test;

import fr.landel.utils.commons.HexUtils;

/**
 * Check hexadecimal utils
 *
 * @since Dec 11, 2015
 * @author Gilles Landel
 *
 */
public class HexUtilsTest {

    /**
     * Test method for {@link fr.landel.utils.commons.HexUtils#intToByte(int)} .
     */
    @Test
    public void testIntToByte() {
        final int expected = 40;
        final int input = 32;

        assertEquals((byte) expected, HexUtils.intToByte(input));
    }

    /**
     * Test method for {@link fr.landel.utils.commons.HexUtils#intToBytes(int)} .
     */
    @Test
    public void testIntToBytes() {
        final byte[] expected = {-128, 18, -20, 74};
        final int input = 1256985216;

        assertTrue(Arrays.equals(expected, HexUtils.intToBytes(input)));
    }

    /**
     * Test method for
     * {@link fr.landel.utils.commons.HexUtils#byteArrayToHexString(byte[])} .
     */
    @Test
    public void testByteArrayToHexString() {
        byte[] bytes = {'A', 'B', 'C'};

        assertEquals("414243", HexUtils.byteArrayToHexString(bytes));
    }
}
