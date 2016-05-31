/*
 * #%L
 * utils-commons
 * %%
 * Copyright (C) 2016 Gilandel
 * %%
 * Authors: Gilles Landel
 * URL: https://github.com/Gilandel
 * 
 * This file is under Apache License, version 2.0 (2004).
 * #L%
 */
package fr.landel.utils.commons;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;

import java.math.BigDecimal;
import java.math.BigInteger;

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
        assertTrue(NumberUtils.isEqual(0.00012d, 0.00013d, null));
        assertTrue(NumberUtils.isEqual((Double) null, (Double) null, 5));
        assertFalse(NumberUtils.isEqual(0.00013d, (Double) null, 5));
        assertFalse(NumberUtils.isEqual((Double) null, 0.00013d, 5));
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
        assertFalse(NumberUtils.isEqual(0.00012f, 0.00013f, null));
        assertTrue(NumberUtils.isEqual((Float) null, (Float) null, 5));
        assertFalse(NumberUtils.isEqual(0.00013f, (Float) null, 5));
        assertFalse(NumberUtils.isEqual((Float) null, 0.00013f, 5));
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

    /**
     * Test method for
     * {@link fr.landel.utils.commons.NumberUtils#round(java.lang.Double)}.
     */
    @Test
    public void testRound() {
        final double val = 10.358941684565d;
        final float expected = 10.4f;

        assertNull(NumberUtils.round(null));
        assertTrue(NumberUtils.isEqual(expected, NumberUtils.round(val), 1));
    }

    /**
     * Test method for
     * {@link fr.landel.utils.commons.NumberUtils#parseInt(java.lang.String)}.
     */
    @Test
    public void testParseIntString() {
        final int expected = 10;

        assertNull(NumberUtils.parseInt(null));
        assertNull(NumberUtils.parseInt(""));
        assertNull(NumberUtils.parseInt("   SS "));
        assertNull(NumberUtils.parseInt("  12 "));

        assertEquals(expected, (int) NumberUtils.parseInt("10"));
        assertEquals(-expected, (int) NumberUtils.parseInt("-10"));
        assertEquals(expected, (int) NumberUtils.parseInt("+10"));
        assertNull(NumberUtils.parseInt("10L"));
        assertNull(NumberUtils.parseInt("10.0"));
        assertNull(NumberUtils.parseInt("10.0D"));
        assertNull(NumberUtils.parseInt("10.0F"));
    }

    /**
     * Test method for
     * {@link fr.landel.utils.commons.NumberUtils#parseInt(java.lang.String, java.lang.Integer)}
     * .
     */
    @Test
    public void testParseIntStringInteger() {
        final int value = 10;
        final int expectedIfNullOrFalse = 10;
        final int expected = 10;

        assertEquals(expectedIfNullOrFalse, (int) NumberUtils.parseInt(null, value));
        assertEquals(expectedIfNullOrFalse, (int) NumberUtils.parseInt("", value));
        assertEquals(expectedIfNullOrFalse, (int) NumberUtils.parseInt("   SS ", value));
        assertEquals(expectedIfNullOrFalse, (int) NumberUtils.parseInt("  12 ", value));

        assertEquals(expected, (int) NumberUtils.parseInt("10", value));
        assertEquals(-expected, (int) NumberUtils.parseInt("-10", value));
        assertEquals(expected, (int) NumberUtils.parseInt("+10", value));
        assertEquals(expectedIfNullOrFalse, (int) NumberUtils.parseInt("10L", value));
        assertEquals(expectedIfNullOrFalse, (int) NumberUtils.parseInt("10.0", value));
        assertEquals(expectedIfNullOrFalse, (int) NumberUtils.parseInt("10.0D", value));
        assertEquals(expectedIfNullOrFalse, (int) NumberUtils.parseInt("10.0F", value));
    }

    /**
     * Test method for
     * {@link fr.landel.utils.commons.NumberUtils#parseFloat(java.lang.String)}.
     */
    @Test
    public void testParseFloatString() {
        final float delta = 0.001f;
        final float expected = 10.0f;

        assertNull(NumberUtils.parseFloat(null));
        assertNull(NumberUtils.parseFloat(""));
        assertNull(NumberUtils.parseFloat("   SS "));
        assertNull(NumberUtils.parseFloat("  12 "));

        assertEquals(expected, (float) NumberUtils.parseFloat("10"), delta);
        assertEquals(-expected, (float) NumberUtils.parseFloat("-10"), delta);
        assertNull(NumberUtils.parseFloat("10L"));
        assertEquals(expected, (float) NumberUtils.parseFloat("10.0"), delta);
        assertNull(NumberUtils.parseFloat("10.0D"));
        assertNull(NumberUtils.parseFloat("10.0F"));
    }

    /**
     * Test method for
     * {@link fr.landel.utils.commons.NumberUtils#parseFloat(java.lang.String, java.lang.Float)}
     * .
     */
    @Test
    public void testParseFloatStringFloat() {
        final float delta = 0.001f;
        final float value = 10.0f;
        final float expectedIfNullOrFalse = 10.0f;
        final float expected = 10.0f;

        assertEquals(expectedIfNullOrFalse, (float) NumberUtils.parseFloat(null, value), delta);
        assertEquals(expectedIfNullOrFalse, (float) NumberUtils.parseFloat("", value), delta);
        assertEquals(expectedIfNullOrFalse, (float) NumberUtils.parseFloat("   SS ", value), delta);
        assertEquals(expectedIfNullOrFalse, (float) NumberUtils.parseFloat("  12 ", value), delta);

        assertEquals(expected, (float) NumberUtils.parseFloat("10", value), delta);
        assertEquals(-expected, (float) NumberUtils.parseFloat("-10", value), delta);
        assertEquals(expectedIfNullOrFalse, (float) NumberUtils.parseFloat("10L", value), delta);
        assertEquals(expected, (float) NumberUtils.parseFloat("10.0", value), delta);
        assertEquals(expected, (float) NumberUtils.parseFloat("+10.0", value), delta);
        assertEquals(expectedIfNullOrFalse, (float) NumberUtils.parseFloat("10.0D", value), delta);
        assertEquals(expectedIfNullOrFalse, (float) NumberUtils.parseFloat("10.0F", value), delta);
    }

    /**
     * Test method for
     * {@link fr.landel.utils.commons.NumberUtils#parseLong(java.lang.String)}.
     */
    @Test
    public void testParseLongString() {
        final long expected = 10L;

        assertNull(NumberUtils.parseLong(null));
        assertNull(NumberUtils.parseLong(""));
        assertNull(NumberUtils.parseLong("   SS "));
        assertNull(NumberUtils.parseLong("  12 "));

        assertEquals(expected, (long) NumberUtils.parseLong("10"));
        assertEquals(-expected, (long) NumberUtils.parseLong("-10"));
        assertNull(NumberUtils.parseLong("10L"));
        assertNull(NumberUtils.parseLong("10.0"));
        assertNull(NumberUtils.parseLong("10.0D"));
        assertNull(NumberUtils.parseLong("10.0F"));
    }

    /**
     * Test method for
     * {@link fr.landel.utils.commons.NumberUtils#parseLong(java.lang.String, java.lang.Long)}
     * .
     */
    @Test
    public void testParseLongStringLong() {
        final long value = 10L;
        final long expectedIfNullOrFalse = 10L;
        final long expected = 10L;

        assertEquals(expectedIfNullOrFalse, (long) NumberUtils.parseLong(null, value));
        assertEquals(expectedIfNullOrFalse, (long) NumberUtils.parseLong("", value));
        assertEquals(expectedIfNullOrFalse, (long) NumberUtils.parseLong("   SS ", value));
        assertEquals(expectedIfNullOrFalse, (long) NumberUtils.parseLong("  12 ", value));

        assertEquals(expected, (long) NumberUtils.parseLong("10", value));
        assertEquals(-expected, (long) NumberUtils.parseLong("-10", value));
        assertEquals(expected, (long) NumberUtils.parseLong("+10", value));
        assertEquals(expectedIfNullOrFalse, (long) NumberUtils.parseLong("10L", value));
        assertEquals(expectedIfNullOrFalse, (long) NumberUtils.parseLong("10.0", value));
        assertEquals(expectedIfNullOrFalse, (long) NumberUtils.parseLong("10.0D", value));
        assertEquals(expectedIfNullOrFalse, (long) NumberUtils.parseLong("10.0F", value));
    }

    /**
     * Test method for
     * {@link fr.landel.utils.commons.NumberUtils#parseDouble(java.lang.String)}.
     */
    @Test
    public void testParseDoubleString() {
        final double delta = 0.001d;
        final double expected = 10.0d;

        assertNull(NumberUtils.parseDouble(null));
        assertNull(NumberUtils.parseDouble(""));
        assertNull(NumberUtils.parseDouble("   SS "));
        assertNull(NumberUtils.parseDouble("  12 "));

        assertEquals(expected, (double) NumberUtils.parseDouble("10"), delta);
        assertEquals(-expected, (double) NumberUtils.parseDouble("-10"), delta);
        assertNull(NumberUtils.parseDouble("10L"));
        assertEquals(expected, (double) NumberUtils.parseDouble("10.0"), delta);
        assertNull(NumberUtils.parseDouble("10.0D"));
        assertNull(NumberUtils.parseDouble("10.0F"));
    }

    /**
     * Test method for
     * {@link fr.landel.utils.commons.NumberUtils#parseDouble(java.lang.String, java.lang.Double)}
     * .
     */
    @Test
    public void testParseDoubleStringDouble() {
        final double delta = 0.001d;
        final double value = 10.0d;
        final double expectedIfNullOrFalse = 10.0d;
        final double expected = 10.0d;

        assertEquals(expectedIfNullOrFalse, (double) NumberUtils.parseDouble(null, value), delta);
        assertEquals(expectedIfNullOrFalse, (double) NumberUtils.parseDouble("", value), delta);
        assertEquals(expectedIfNullOrFalse, (double) NumberUtils.parseDouble("   SS ", value), delta);
        assertEquals(expectedIfNullOrFalse, (double) NumberUtils.parseDouble("  12 ", value), delta);

        assertEquals(expected, (double) NumberUtils.parseDouble("10", value), delta);
        assertEquals(-expected, (double) NumberUtils.parseDouble("-10", value), delta);
        assertEquals(expectedIfNullOrFalse, (double) NumberUtils.parseDouble("10L", value), delta);
        assertEquals(expected, (double) NumberUtils.parseDouble("10.0", value), delta);
        assertEquals(expectedIfNullOrFalse, (double) NumberUtils.parseDouble("10.0D", value), delta);
        assertEquals(expectedIfNullOrFalse, (double) NumberUtils.parseDouble("10.0F", value), delta);
    }

    /**
     * Test method for
     * {@link fr.landel.utils.commons.NumberUtils#parseShort(java.lang.String)}.
     */
    @Test
    public void testParseShortString() {
        final short expected = 10;

        assertNull(NumberUtils.parseShort(null));
        assertNull(NumberUtils.parseShort(""));
        assertNull(NumberUtils.parseShort("   SS "));
        assertNull(NumberUtils.parseShort("  12 "));

        assertEquals(expected, (short) NumberUtils.parseShort("10"));
        assertEquals(-expected, (short) NumberUtils.parseShort("-10"));
        assertNull(NumberUtils.parseShort("10L"));
        assertNull(NumberUtils.parseShort("10.0"));
        assertNull(NumberUtils.parseShort("10.0D"));
        assertNull(NumberUtils.parseShort("10.0F"));
    }

    /**
     * Test method for
     * {@link fr.landel.utils.commons.NumberUtils#parseShort(java.lang.String, java.lang.Short)}
     * .
     */
    @Test
    public void testParseShortStringShort() {
        final short value = 10;
        final short expectedIfNullOrFalse = 10;
        final short expected = 10;

        assertEquals(expectedIfNullOrFalse, (short) NumberUtils.parseShort(null, value));
        assertEquals(expectedIfNullOrFalse, (short) NumberUtils.parseShort("", value));
        assertEquals(expectedIfNullOrFalse, (short) NumberUtils.parseShort("   SS ", value));
        assertEquals(expectedIfNullOrFalse, (short) NumberUtils.parseShort("  12 ", value));

        assertEquals(expected, (short) NumberUtils.parseShort("10", value));
        assertEquals(-expected, (short) NumberUtils.parseShort("-10", value));
        assertEquals(expected, (short) NumberUtils.parseShort("+10", value));
        assertEquals(expectedIfNullOrFalse, (short) NumberUtils.parseShort(String.valueOf(Short.MAX_VALUE + 1), value));
        assertEquals(expectedIfNullOrFalse, (short) NumberUtils.parseShort(String.valueOf(Short.MIN_VALUE - 1), value));
        assertEquals(expectedIfNullOrFalse, (short) NumberUtils.parseShort("10L", value));
        assertEquals(expectedIfNullOrFalse, (short) NumberUtils.parseShort("10L", value));
        assertEquals(expectedIfNullOrFalse, (short) NumberUtils.parseShort("10.0", value));
        assertEquals(expectedIfNullOrFalse, (short) NumberUtils.parseShort("10.0D", value));
        assertEquals(expectedIfNullOrFalse, (short) NumberUtils.parseShort("10.0F", value));
    }

    /**
     * Test method for
     * {@link fr.landel.utils.commons.NumberUtils#parseByte(java.lang.String)}.
     */
    @Test
    public void testParseByteString() {
        final byte expected = 10;

        assertNull(NumberUtils.parseByte(null));
        assertNull(NumberUtils.parseByte(""));
        assertNull(NumberUtils.parseByte("   SS "));
        assertNull(NumberUtils.parseByte("  12 "));

        assertEquals(expected, (byte) NumberUtils.parseByte("10"));
        assertEquals(-expected, (byte) NumberUtils.parseByte("-10"));
        assertNull(NumberUtils.parseByte("10L"));
        assertNull(NumberUtils.parseByte("10.0"));
        assertNull(NumberUtils.parseByte("10.0D"));
        assertNull(NumberUtils.parseByte("10.0F"));
    }

    /**
     * Test method for
     * {@link fr.landel.utils.commons.NumberUtils#parseByte(java.lang.String, java.lang.Byte)}
     * .
     */
    @Test
    public void testParseByteStringByte() {
        final byte value = 10;
        final byte expectedIfNullOrFalse = 10;
        final byte expected = 10;

        assertEquals(expectedIfNullOrFalse, (byte) NumberUtils.parseByte(null, value));
        assertEquals(expectedIfNullOrFalse, (byte) NumberUtils.parseByte("", value));
        assertEquals(expectedIfNullOrFalse, (byte) NumberUtils.parseByte("   SS ", value));
        assertEquals(expectedIfNullOrFalse, (byte) NumberUtils.parseByte("  12 ", value));

        assertEquals(expected, (byte) NumberUtils.parseByte("10", value));
        assertEquals(-expected, (byte) NumberUtils.parseByte("-10", value));
        assertEquals(expected, (byte) NumberUtils.parseByte("+10", value));
        assertEquals(expectedIfNullOrFalse, (short) NumberUtils.parseByte(String.valueOf(Byte.MAX_VALUE + 1), value));
        assertEquals(expectedIfNullOrFalse, (short) NumberUtils.parseByte(String.valueOf(Byte.MIN_VALUE - 1), value));
        assertEquals(expectedIfNullOrFalse, (short) NumberUtils.parseByte("10L", value));
        assertEquals(expectedIfNullOrFalse, (short) NumberUtils.parseByte("10L", value));
        assertEquals(expectedIfNullOrFalse, (short) NumberUtils.parseByte("10.0", value));
        assertEquals(expectedIfNullOrFalse, (short) NumberUtils.parseByte("10.0D", value));
        assertEquals(expectedIfNullOrFalse, (short) NumberUtils.parseByte("10.0F", value));
    }

    /**
     * Test method for
     * {@link fr.landel.utils.commons.NumberUtils#isByte(java.lang.Number)} .
     */
    @Test
    public void testNumber() {
        Number numByte = (byte) 12;
        Number numShort = (short) 12;
        Number numInteger = (int) 12;
        Number numLong = 12L;
        Number numFloat = 12.0f;
        Number numDouble = 12.0d;
        Number numBigInteger = BigInteger.valueOf(12L);
        Number numBigDecimal = BigDecimal.valueOf(12.0d);

        assertTrue(NumberUtils.isByte(numByte));
        assertFalse(NumberUtils.isShort(numByte));
        assertFalse(NumberUtils.isInteger(numByte));
        assertFalse(NumberUtils.isLong(numByte));
        assertFalse(NumberUtils.isFloat(numByte));
        assertFalse(NumberUtils.isDouble(numByte));
        assertFalse(NumberUtils.isBigInteger(numByte));
        assertFalse(NumberUtils.isBigDecimal(numByte));

        assertFalse(NumberUtils.isByte(numShort));
        assertTrue(NumberUtils.isShort(numShort));
        assertFalse(NumberUtils.isInteger(numShort));
        assertFalse(NumberUtils.isLong(numShort));
        assertFalse(NumberUtils.isFloat(numShort));
        assertFalse(NumberUtils.isDouble(numShort));
        assertFalse(NumberUtils.isBigInteger(numShort));
        assertFalse(NumberUtils.isBigDecimal(numShort));

        assertFalse(NumberUtils.isByte(numInteger));
        assertFalse(NumberUtils.isShort(numInteger));
        assertTrue(NumberUtils.isInteger(numInteger));
        assertFalse(NumberUtils.isLong(numInteger));
        assertFalse(NumberUtils.isFloat(numInteger));
        assertFalse(NumberUtils.isDouble(numInteger));
        assertFalse(NumberUtils.isBigInteger(numInteger));
        assertFalse(NumberUtils.isBigDecimal(numInteger));

        assertFalse(NumberUtils.isByte(numLong));
        assertFalse(NumberUtils.isShort(numLong));
        assertFalse(NumberUtils.isInteger(numLong));
        assertTrue(NumberUtils.isLong(numLong));
        assertFalse(NumberUtils.isFloat(numLong));
        assertFalse(NumberUtils.isDouble(numLong));
        assertFalse(NumberUtils.isBigInteger(numLong));
        assertFalse(NumberUtils.isBigDecimal(numLong));

        assertFalse(NumberUtils.isByte(numFloat));
        assertFalse(NumberUtils.isShort(numFloat));
        assertFalse(NumberUtils.isInteger(numFloat));
        assertFalse(NumberUtils.isLong(numFloat));
        assertTrue(NumberUtils.isFloat(numFloat));
        assertFalse(NumberUtils.isDouble(numFloat));
        assertFalse(NumberUtils.isBigInteger(numFloat));
        assertFalse(NumberUtils.isBigDecimal(numFloat));

        assertFalse(NumberUtils.isByte(numDouble));
        assertFalse(NumberUtils.isShort(numDouble));
        assertFalse(NumberUtils.isInteger(numDouble));
        assertFalse(NumberUtils.isLong(numDouble));
        assertFalse(NumberUtils.isFloat(numDouble));
        assertTrue(NumberUtils.isDouble(numDouble));
        assertFalse(NumberUtils.isBigInteger(numDouble));
        assertFalse(NumberUtils.isBigDecimal(numDouble));

        assertFalse(NumberUtils.isByte(numBigInteger));
        assertFalse(NumberUtils.isShort(numBigInteger));
        assertFalse(NumberUtils.isInteger(numBigInteger));
        assertFalse(NumberUtils.isLong(numBigInteger));
        assertFalse(NumberUtils.isFloat(numBigInteger));
        assertFalse(NumberUtils.isDouble(numBigInteger));
        assertTrue(NumberUtils.isBigInteger(numBigInteger));
        assertFalse(NumberUtils.isBigDecimal(numBigInteger));

        assertFalse(NumberUtils.isByte(numBigDecimal));
        assertFalse(NumberUtils.isShort(numBigDecimal));
        assertFalse(NumberUtils.isInteger(numBigDecimal));
        assertFalse(NumberUtils.isLong(numBigDecimal));
        assertFalse(NumberUtils.isFloat(numBigDecimal));
        assertFalse(NumberUtils.isDouble(numBigDecimal));
        assertFalse(NumberUtils.isBigInteger(numBigDecimal));
        assertTrue(NumberUtils.isBigDecimal(numBigDecimal));
    }
}
