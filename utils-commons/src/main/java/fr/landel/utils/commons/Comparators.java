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

import java.math.BigDecimal;
import java.math.BigInteger;
import java.util.Comparator;

/**
 * Comparators list (byte, short, integer, long, float, double, big integer, big
 * decimal, character and string)
 *
 * @since 26 juin 2016
 * @author Gilles
 *
 */
public class Comparators {

    /**
     * Byte comparator (handle null)
     */
    public static final Comparator<Byte> BYTE = new Comparator<Byte>() {
        @Override
        public int compare(final Byte o1, final Byte o2) {
            return Comparators.compare(o1, o2);
        }
    };

    /**
     * Reverse byte comparator (handle null)
     */
    public static final Comparator<Byte> BYTE_REVERSE = new Comparator<Byte>() {
        @Override
        public int compare(final Byte o1, final Byte o2) {
            return Comparators.compareReverse(o1, o2);
        }
    };

    /**
     * Short comparator (handle null)
     */
    public static final Comparator<Short> SHORT = new Comparator<Short>() {
        @Override
        public int compare(final Short o1, final Short o2) {
            return Comparators.compare(o1, o2);
        }
    };

    /**
     * Reverse short comparator (handle null)
     */
    public static final Comparator<Short> SHORT_REVERSE = new Comparator<Short>() {
        @Override
        public int compare(final Short o1, final Short o2) {
            return Comparators.compareReverse(o1, o2);
        }
    };

    /**
     * Integer comparator (handle null)
     */
    public static final Comparator<Integer> INTEGER = new Comparator<Integer>() {
        @Override
        public int compare(final Integer o1, final Integer o2) {
            return Comparators.compare(o1, o2);
        }
    };

    /**
     * Reverse integer comparator (handle null)
     */
    public static final Comparator<Integer> INTEGER_REVERSE = new Comparator<Integer>() {
        @Override
        public int compare(final Integer o1, final Integer o2) {
            return Comparators.compareReverse(o1, o2);
        }
    };

    /**
     * Long comparator (handle null)
     */
    public static final Comparator<Long> LONG = new Comparator<Long>() {
        @Override
        public int compare(final Long o1, final Long o2) {
            return Comparators.compare(o1, o2);
        }
    };

    /**
     * Reverse long comparator (handle null)
     */
    public static final Comparator<Long> LONG_REVERSE = new Comparator<Long>() {
        @Override
        public int compare(final Long o1, final Long o2) {
            return Comparators.compareReverse(o1, o2);
        }
    };

    /**
     * Float comparator (handle null)
     */
    public static final Comparator<Float> FLOAT = new Comparator<Float>() {
        @Override
        public int compare(final Float o1, final Float o2) {
            return Comparators.compare(o1, o2);
        }
    };

    /**
     * Reverse float comparator (handle null)
     */
    public static final Comparator<Float> FLOAT_REVERSE = new Comparator<Float>() {
        @Override
        public int compare(final Float o1, final Float o2) {
            return Comparators.compareReverse(o1, o2);
        }
    };

    /**
     * Double comparator (handle null)
     */
    public static final Comparator<Double> DOUBLE = new Comparator<Double>() {
        @Override
        public int compare(final Double o1, final Double o2) {
            return Comparators.compare(o1, o2);
        }
    };

    /**
     * Reverse double comparator (handle null)
     */
    public static final Comparator<Double> DOUBLE_REVERSE = new Comparator<Double>() {
        @Override
        public int compare(final Double o1, final Double o2) {
            return Comparators.compareReverse(o1, o2);
        }
    };

    /**
     * Big integer comparator (handle null)
     */
    public static final Comparator<BigInteger> BIG_INTEGER = new Comparator<BigInteger>() {
        @Override
        public int compare(final BigInteger o1, final BigInteger o2) {
            return Comparators.compare(o1, o2);
        }
    };

    /**
     * Reverse big integer comparator (handle null)
     */
    public static final Comparator<BigInteger> BIG_INTEGER_REVERSE = new Comparator<BigInteger>() {
        @Override
        public int compare(final BigInteger o1, final BigInteger o2) {
            return Comparators.compareReverse(o1, o2);
        }
    };

    /**
     * Big decimal comparator (handle null)
     */
    public static final Comparator<BigDecimal> BIG_DECIMAL = new Comparator<BigDecimal>() {
        @Override
        public int compare(final BigDecimal o1, final BigDecimal o2) {
            return Comparators.compare(o1, o2);
        }
    };

    /**
     * Reverse big decimal comparator (handle null)
     */
    public static final Comparator<BigDecimal> BIG_DECIMAL_REVERSE = new Comparator<BigDecimal>() {
        @Override
        public int compare(final BigDecimal o1, final BigDecimal o2) {
            return Comparators.compareReverse(o1, o2);
        }
    };

    /**
     * Character comparator (handle null)
     */
    public static final Comparator<Character> CHAR = new Comparator<Character>() {
        @Override
        public int compare(final Character o1, final Character o2) {
            return Comparators.compare(o1, o2);
        }
    };

    /**
     * Reverse character comparator (handle null)
     */
    public static final Comparator<Character> CHAR_REVERSE = new Comparator<Character>() {
        @Override
        public int compare(final Character o1, final Character o2) {
            return Comparators.compareReverse(o1, o2);
        }
    };

    /**
     * String comparator (handle null)
     */
    public static final Comparator<String> STRING = new Comparator<String>() {
        @Override
        public int compare(final String o1, final String o2) {
            return Comparators.compare(o1, o2);
        }
    };

    /**
     * Reverse string comparator (handle null)
     */
    public static final Comparator<String> STRING_REVERSE = new Comparator<String>() {
        @Override
        public int compare(final String o1, final String o2) {
            return Comparators.compareReverse(o1, o2);
        }
    };

    private static <T extends Comparable<T>> int compare(final T o1, final T o2) {
        if (o1 == null) {
            return Integer.MIN_VALUE;
        } else if (o2 == null) {
            return Integer.MAX_VALUE;
        }
        return o1.compareTo(o2);
    }

    private static <T extends Comparable<T>> int compareReverse(final T o1, final T o2) {
        if (o1 == null) {
            return Integer.MAX_VALUE;
        } else if (o2 == null) {
            return Integer.MIN_VALUE;
        }
        return o2.compareTo(o1);
    }
}