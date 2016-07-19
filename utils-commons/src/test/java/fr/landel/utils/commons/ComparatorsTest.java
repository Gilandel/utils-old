/*-
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

import java.math.BigDecimal;
import java.math.BigInteger;
import java.util.Arrays;
import java.util.List;
import java.util.stream.Collectors;

import org.junit.Test;

/**
 * Check comparators
 *
 * @since 28 juin 2016
 * @author Gilles
 *
 */
public class ComparatorsTest {

    /**
     * {@link Comparators#BYTE} {@link Comparators#BYTE_REVERSE}
     */
    @Test
    public void testByte() {
        List<Byte> bytes = Arrays.asList((byte) 5, (byte) 2, (byte) 3);

        List<Byte> bytesSorted = bytes.stream().sorted(Comparators.BYTE.asc()).collect(Collectors.toList());

        assertEquals(Byte.valueOf((byte) 2), bytesSorted.get(0));
        assertEquals(Byte.valueOf((byte) 3), bytesSorted.get(1));
        assertEquals(Byte.valueOf((byte) 5), bytesSorted.get(2));

        bytesSorted = bytes.stream().sorted(Comparators.BYTE.desc()).collect(Collectors.toList());

        assertEquals(Byte.valueOf((byte) 2), bytesSorted.get(2));
        assertEquals(Byte.valueOf((byte) 3), bytesSorted.get(1));
        assertEquals(Byte.valueOf((byte) 5), bytesSorted.get(0));

        assertEquals(Integer.MIN_VALUE, Comparators.BYTE.asc().compare(null, (byte) 5));
        assertEquals(Integer.MAX_VALUE, Comparators.BYTE.asc().compare((byte) 5, null));

        assertEquals(Integer.MAX_VALUE, Comparators.BYTE.desc().compare(null, (byte) 5));
        assertEquals(Integer.MIN_VALUE, Comparators.BYTE.desc().compare((byte) 5, null));
    }

    /**
     * {@link Comparators#SHORT} {@link Comparators#SHORT_REVERSE}
     */
    @Test
    public void testShort() {
        List<Short> shorts = Arrays.asList((short) 5, (short) 2, (short) 3);

        List<Short> shortsSorted = shorts.stream().sorted(Comparators.SHORT.asc()).collect(Collectors.toList());

        assertEquals(Short.valueOf((short) 2), shortsSorted.get(0));
        assertEquals(Short.valueOf((short) 3), shortsSorted.get(1));
        assertEquals(Short.valueOf((short) 5), shortsSorted.get(2));

        shortsSorted = shorts.stream().sorted(Comparators.SHORT.desc()).collect(Collectors.toList());

        assertEquals(Short.valueOf((short) 2), shortsSorted.get(2));
        assertEquals(Short.valueOf((short) 3), shortsSorted.get(1));
        assertEquals(Short.valueOf((short) 5), shortsSorted.get(0));
    }

    /**
     * {@link Comparators#CHAR} {@link Comparators#CHAR_REVERSE}
     */
    @Test
    public void testChar() {
        List<Character> chars = Arrays.asList((char) 5, (char) 2, (char) 3);

        List<Character> charsSorted = chars.stream().sorted(Comparators.CHAR.asc()).collect(Collectors.toList());

        assertEquals(Character.valueOf((char) 2), charsSorted.get(0));
        assertEquals(Character.valueOf((char) 3), charsSorted.get(1));
        assertEquals(Character.valueOf((char) 5), charsSorted.get(2));

        charsSorted = chars.stream().sorted(Comparators.CHAR.desc()).collect(Collectors.toList());

        assertEquals(Character.valueOf((char) 2), charsSorted.get(2));
        assertEquals(Character.valueOf((char) 3), charsSorted.get(1));
        assertEquals(Character.valueOf((char) 5), charsSorted.get(0));
    }

    /**
     * {@link Comparators#INTEGER} {@link Comparators#INTEGER_REVERSE}
     */
    @Test
    public void testInteger() {
        List<Integer> integers = Arrays.asList(5, 2, 3);

        List<Integer> integersSorted = integers.stream().sorted(Comparators.INTEGER.asc()).collect(Collectors.toList());

        assertEquals(Integer.valueOf(2), integersSorted.get(0));
        assertEquals(Integer.valueOf(3), integersSorted.get(1));
        assertEquals(Integer.valueOf(5), integersSorted.get(2));

        integersSorted = integers.stream().sorted(Comparators.INTEGER.desc()).collect(Collectors.toList());

        assertEquals(Integer.valueOf(2), integersSorted.get(2));
        assertEquals(Integer.valueOf(3), integersSorted.get(1));
        assertEquals(Integer.valueOf(5), integersSorted.get(0));
    }

    /**
     * {@link Comparators#LONG} {@link Comparators#LONG_REVERSE}
     */
    @Test
    public void testLong() {
        List<Long> longs = Arrays.asList(5L, 2L, 3L);

        List<Long> longsSorted = longs.stream().sorted(Comparators.LONG.asc()).collect(Collectors.toList());

        assertEquals(Long.valueOf(2), longsSorted.get(0));
        assertEquals(Long.valueOf(3), longsSorted.get(1));
        assertEquals(Long.valueOf(5), longsSorted.get(2));

        longsSorted = longs.stream().sorted(Comparators.LONG.desc()).collect(Collectors.toList());

        assertEquals(Long.valueOf(2), longsSorted.get(2));
        assertEquals(Long.valueOf(3), longsSorted.get(1));
        assertEquals(Long.valueOf(5), longsSorted.get(0));
    }

    /**
     * {@link Comparators#FLOAT} {@link Comparators#FLOAT_REVERSE}
     */
    @Test
    public void testFloat() {
        List<Float> floats = Arrays.asList(5f, 2f, 3f);

        List<Float> floatsSorted = floats.stream().sorted(Comparators.FLOAT.asc()).collect(Collectors.toList());

        assertEquals(Float.valueOf(2f), floatsSorted.get(0));
        assertEquals(Float.valueOf(3f), floatsSorted.get(1));
        assertEquals(Float.valueOf(5f), floatsSorted.get(2));

        floatsSorted = floats.stream().sorted(Comparators.FLOAT.desc()).collect(Collectors.toList());

        assertEquals(Float.valueOf(2f), floatsSorted.get(2));
        assertEquals(Float.valueOf(3f), floatsSorted.get(1));
        assertEquals(Float.valueOf(5f), floatsSorted.get(0));
    }

    /**
     * {@link Comparators#DOUBLE} {@link Comparators#DOUBLE_REVERSE}
     */
    @Test
    public void testDouble() {
        List<Double> doubles = Arrays.asList(5d, 2d, 3d);

        List<Double> doublesSorted = doubles.stream().sorted(Comparators.DOUBLE.asc()).collect(Collectors.toList());

        assertEquals(Double.valueOf(2d), doublesSorted.get(0));
        assertEquals(Double.valueOf(3d), doublesSorted.get(1));
        assertEquals(Double.valueOf(5d), doublesSorted.get(2));

        doublesSorted = doubles.stream().sorted(Comparators.DOUBLE.desc()).collect(Collectors.toList());

        assertEquals(Double.valueOf(2d), doublesSorted.get(2));
        assertEquals(Double.valueOf(3d), doublesSorted.get(1));
        assertEquals(Double.valueOf(5d), doublesSorted.get(0));
    }

    /**
     * {@link Comparators#BIG_INTEGER} {@link Comparators#BIG_INTEGER_REVERSE}
     */
    @Test
    public void testBigInteger() {
        List<BigInteger> bigIntegers = Arrays.asList(new BigInteger("5"), new BigInteger("2"), new BigInteger("3"));

        List<BigInteger> bigIntegersSorted = bigIntegers.stream().sorted(Comparators.BIG_INTEGER.asc()).collect(Collectors.toList());

        assertEquals(BigInteger.valueOf(2l), bigIntegersSorted.get(0));
        assertEquals(BigInteger.valueOf(3l), bigIntegersSorted.get(1));
        assertEquals(BigInteger.valueOf(5l), bigIntegersSorted.get(2));

        bigIntegersSorted = bigIntegers.stream().sorted(Comparators.BIG_INTEGER.desc()).collect(Collectors.toList());

        assertEquals(BigInteger.valueOf(2l), bigIntegersSorted.get(2));
        assertEquals(BigInteger.valueOf(3l), bigIntegersSorted.get(1));
        assertEquals(BigInteger.valueOf(5l), bigIntegersSorted.get(0));
    }

    /**
     * {@link Comparators#BIG_DECIMAL} {@link Comparators#BIG_DECIMAL_REVERSE}
     */
    @Test
    public void testBigDecimal() {
        List<BigDecimal> bigDecimals = Arrays.asList(BigDecimal.valueOf(5d), BigDecimal.valueOf(2d), BigDecimal.valueOf(3d));

        List<BigDecimal> bigDecimalsSorted = bigDecimals.stream().sorted(Comparators.BIG_DECIMAL.asc()).collect(Collectors.toList());

        assertEquals(BigDecimal.valueOf(2d), bigDecimalsSorted.get(0));
        assertEquals(BigDecimal.valueOf(3d), bigDecimalsSorted.get(1));
        assertEquals(BigDecimal.valueOf(5d), bigDecimalsSorted.get(2));

        bigDecimalsSorted = bigDecimals.stream().sorted(Comparators.BIG_DECIMAL.desc()).collect(Collectors.toList());

        assertEquals(BigDecimal.valueOf(2d), bigDecimalsSorted.get(2));
        assertEquals(BigDecimal.valueOf(3d), bigDecimalsSorted.get(1));
        assertEquals(BigDecimal.valueOf(5d), bigDecimalsSorted.get(0));
    }

    /**
     * {@link Comparators#STRING} {@link Comparators#STRING_REVERSE}
     */
    @Test
    public void testString() {
        List<String> strings = Arrays.asList("a1e", "a1a", "a1b");

        List<String> stringsSorted = strings.stream().sorted(Comparators.STRING.asc()).collect(Collectors.toList());

        assertEquals("a1a", stringsSorted.get(0));
        assertEquals("a1b", stringsSorted.get(1));
        assertEquals("a1e", stringsSorted.get(2));

        stringsSorted = strings.stream().sorted(Comparators.STRING.desc()).collect(Collectors.toList());

        assertEquals("a1a", stringsSorted.get(2));
        assertEquals("a1b", stringsSorted.get(1));
        assertEquals("a1e", stringsSorted.get(0));
    }
}
