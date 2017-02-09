/*-
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

import java.math.BigDecimal;
import java.math.BigInteger;
import java.util.Arrays;
import java.util.List;
import java.util.stream.Collectors;

import org.junit.Test;

/**
 * Check comparators
 *
 * @since Jun 28, 2016
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

    /**
     * Check {@link Comparators#VERSION}
     */
    @Test
    public void testVersion1() {
        Version v1 = new Version("");
        Version v2 = new Version("1.2.1");

        assertEquals("", v1.toString());
        assertEquals("1.2.1", v2.toString());

        assertEquals(-1, Comparators.VERSION.asc().compare(v1, v2));

        v1 = new Version("1.2.0");
        v2 = new Version("");

        assertEquals(1, Comparators.VERSION.asc().compare(v1, v2));

        v1 = new Version("");
        v2 = new Version("");

        assertEquals(0, Comparators.VERSION.asc().compare(v1, v2));

        v1 = new Version("1.2.0");
        v2 = new Version("1.2.1");

        assertEquals(-1, Comparators.VERSION.asc().compare(v1, v2));

        v1 = new Version("1.2.0");
        v2 = new Version("1.2");

        assertEquals(0, Comparators.VERSION.asc().compare(v1, v2));

        v1 = new Version("1.2");
        v2 = new Version("1.2.0");

        assertEquals(0, Comparators.VERSION.asc().compare(v1, v2));

        v1 = new Version("1.2");
        v2 = new Version("1.2.1");

        assertEquals(-1, Comparators.VERSION.asc().compare(v1, v2));

        v1 = new Version("1.2");
        v2 = new Version("1.2-SNAPSHOT");

        assertEquals(1, Comparators.VERSION.asc().compare(v1, v2));

        v1 = new Version("1.2.0");
        v2 = new Version("1.2-SNAPSHOT");

        assertEquals(1, Comparators.VERSION.asc().compare(v1, v2));
    }

    /**
     * Check {@link Comparators#VERSION}
     */
    @Test
    public void testVersion2() {
        Version v1 = new Version("1.2-SNAPSHOT");
        Version v2 = new Version("1.2.0");

        assertEquals(-1, Comparators.VERSION.asc().compare(v1, v2));

        v1 = new Version("1.2-beta-2");
        v2 = new Version("1.2");

        assertEquals(-1, Comparators.VERSION.asc().compare(v1, v2));

        v1 = new Version("1.2");
        v2 = new Version("1.2-beta-2");

        assertEquals(1, Comparators.VERSION.asc().compare(v1, v2));

        v1 = new Version("1.2-beta-2");
        v2 = new Version("1.2.1");

        assertEquals(-1, Comparators.VERSION.asc().compare(v1, v2));

        v1 = new Version("1.2-beta-2");
        v2 = new Version("1.2-SNAPSHOT");

        assertEquals(1, Comparators.VERSION.asc().compare(v1, v2));

        v1 = new Version("1.2-SNAPSHOT");
        v2 = new Version("1.2-beta-2");

        assertEquals(-1, Comparators.VERSION.asc().compare(v1, v2));

        v1 = new Version("1.2-beta-2");
        v2 = new Version("1.2-alpha-6");

        assertEquals(1, Comparators.VERSION.asc().compare(v1, v2));

        v1 = new Version("1.2.0");
        v2 = new Version("1.2.1");
        Version v3 = new Version("1.2.0-SNAPSHOT");

        List<Version> versions = Arrays.asList(v1, v2, v3);
        List<Version> versionSorted = versions.stream().sorted(Comparators.VERSION.asc()).collect(Collectors.toList());

        assertEquals(v3, versionSorted.get(0));
        assertEquals(v1, versionSorted.get(1));
        assertEquals(v2, versionSorted.get(2));

        v1 = new Version("1.2-beta-2");
        v2 = new Version("1.2-alpha-6");
        v3 = new Version("1.2.0-SNAPSHOT");

        versions = Arrays.asList(v1, v2, v3);
        versionSorted = versions.stream().sorted(Comparators.VERSION.asc()).collect(Collectors.toList());

        assertEquals(v3, versionSorted.get(0));
        assertEquals(v2, versionSorted.get(1));
        assertEquals(v1, versionSorted.get(2));

        versionSorted = versions.stream().sorted(Comparators.VERSION.desc()).collect(Collectors.toList());

        assertEquals(v1, versionSorted.get(0));
        assertEquals(v2, versionSorted.get(1));
        assertEquals(v3, versionSorted.get(2));

        v1 = new Version("1.2");
        v2 = new Version("1.2-beta-2");
        v3 = new Version("1.2-alpha-6");
        Version v4 = new Version("1.2.0-SNAPSHOT");
        Version v5 = new Version("1.2.0");
        Version v6 = new Version("1.2.1");

        versions = Arrays.asList(v1, v2, v3, v4, v5, v6);
        versionSorted = versions.stream().sorted(Comparators.VERSION.asc()).collect(Collectors.toList());

        assertEquals(v4, versionSorted.get(0));
        assertEquals(v5, versionSorted.get(1));
        assertEquals(v3, versionSorted.get(2));
        assertEquals(v2, versionSorted.get(3));
        assertEquals(v1, versionSorted.get(4));
        assertEquals(v6, versionSorted.get(5));
    }

    /**
     * Check {@link Comparators#VERSION}
     */
    @Test(expected = NullPointerException.class)
    public void testVersionNull() {
        new Version(null);
    }

    /**
     * Check {@link Comparators#compare}
     */
    @Test
    public void testCompare() {
        Integer object = Integer.valueOf(12);
        Integer object2 = Integer.valueOf(13);

        assertEquals(0, Comparators.compare(object, object));
        assertEquals(0, Comparators.compare(null, null));
        assertEquals(Integer.MAX_VALUE, Comparators.compare(object, null));
        assertEquals(Integer.MIN_VALUE, Comparators.compare(null, object));
        assertEquals(-1, Comparators.compare(object, object2));
        assertEquals(1, Comparators.compare(object2, object));
    }

    /**
     * Check {@link Comparators#compareReverse}
     */
    @Test
    public void testCompareReverse() {
        Integer object = Integer.valueOf(12);
        Integer object2 = Integer.valueOf(13);

        assertEquals(0, Comparators.compareReverse(object, object));
        assertEquals(0, Comparators.compareReverse(null, null));
        assertEquals(Integer.MIN_VALUE, Comparators.compareReverse(object, null));
        assertEquals(Integer.MAX_VALUE, Comparators.compareReverse(null, object));
        assertEquals(1, Comparators.compareReverse(object, object2));
        assertEquals(-1, Comparators.compareReverse(object2, object));
    }
}
