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

import java.math.BigDecimal;

import org.apache.commons.lang3.StringUtils;

/**
 * Utility class to manage numbers.
 *
 * @since 27 nov. 2015
 * @author Gilles Landel
 *
 */
public final class NumberUtils extends org.apache.commons.lang3.math.NumberUtils {

    private static final int RADIX = 10;
    private static final int TEN = 10;

    /**
     * Get the max decimal length.
     * 
     * @param num1
     *            double
     * @param num2
     *            double
     * @return the max decimal length
     */
    private static Integer getMaxDecimalsLength(final Double num1, final Double num2) {
        return Math.max(BigDecimal.valueOf(num1).scale(), BigDecimal.valueOf(num2).scale());
    }

    /**
     * Get the max decimal length..
     * 
     * @param num1
     *            float
     * @param num2
     *            float
     * @return the max decimal length
     */
    private static Integer getMaxDecimalsLength(final Float num1, final Float num2) {
        return Math.max(BigDecimal.valueOf(num1).scale(), BigDecimal.valueOf(num2).scale());
    }

    /**
     * Check if the two doubles are equal (nullsafe).
     * 
     * @param num1
     *            The first double
     * @param num2
     *            The second double
     * @return true if equals
     */
    public static boolean isEqual(final Double num1, final Double num2) {
        return isEqual(num1, num2, getMaxDecimalsLength(num1, num2));
    }

    /**
     * Check if the two doubles are equal (nullsafe).
     * 
     * @param num1
     *            The first double
     * @param num2
     *            The second double
     * @param accuracy
     *            The accuracy (1/pow(10,accuracy))
     * @return true if equals
     */
    public static boolean isEqual(final Double num1, final Double num2, final Integer accuracy) {
        if (num1 != null && num2 != null) {
            if (accuracy != null) {
                double maxGap = 1d / Math.pow(TEN, accuracy);
                return Math.abs(num1 - num2) < maxGap;
            } else {
                return isEqual(num1, num2);
            }
        }
        return num1 == null && num2 == null;
    }

    /**
     * Check if the two floats are equal (nullsafe).
     * 
     * @param num1
     *            The first float
     * @param num2
     *            The second float
     * @return true if equals
     */
    public static boolean isEqual(final Float num1, final Float num2) {
        return isEqual(num1, num2, getMaxDecimalsLength(num1, num2));
    }

    /**
     * Check if the two floats are equal (nullsafe).
     * 
     * @param num1
     *            The first float
     * @param num2
     *            The second float
     * @param accuracy
     *            The accuracy (1/pow(10,accuracy))
     * @return true if equals
     */
    public static boolean isEqual(final Float num1, final Float num2, final Integer accuracy) {
        if (num1 != null && num2 != null) {
            if (accuracy != null) {
                float maxGap = (float) (1d / Math.pow(TEN, accuracy));
                return Math.abs(num1 - num2) < maxGap;
            } else {
                return isEqual(num1, num2);
            }
        }
        return num1 == null && num2 == null;
    }

    /**
     * Get the number if not null and the default one otherwise.
     * 
     * @param num
     *            The number to check, may be null
     * @param defaultNum
     *            The default number
     * @param <N>
     *            Type of the number
     * @return a number
     */
    public static <N extends Number> N getDefaultIfNull(final N num, final N defaultNum) {
        if (num != null) {
            return num;
        }
        return defaultNum;
    }

    /**
     * Round to 0.x
     * 
     * @param value
     *            The value
     * @return A float
     */
    public static Float round(final Double value) {
        if (value != null) {
            return (float) (Math.round(value * TEN)) / TEN;
        }
        return null;
    }

    public static boolean isNumberInteger(final String string) {
        if (StringUtils.isEmpty(string)) {
            return false;
        }
        int start = 0;
        if (string.startsWith("-") || string.startsWith("+")) {
            start = 1;
        }
        for (int i = start; i < string.length(); i++) {
            if (!Character.isDigit(string.charAt(i))) {
                return false;
            }
        }
        return true;
    }

    public static boolean isNumberDecimal(final String string) {
        if (StringUtils.isEmpty(string)) {
            return false;
        }
        int start = 0;
        if (string.startsWith("-") || string.startsWith("+")) {
            start = 1;
        }
        for (int i = start; i < string.length(); i++) {
            if (!Character.isDigit(string.charAt(i)) && '.' != string.charAt(i)) {
                return false;
            }
        }
        return true;
    }

    /**
     * Parse a string into an integer. (Null safe and number safe). Return null,
     * if the string is null or not a number. Can raise a NumberFormatException,
     * if the number is greater than an integer.
     * 
     * @param string
     *            The input
     * @return The parsed result
     */
    public static Integer parseInt(final String string) {
        return parseInt(string, null, RADIX);
    }

    /**
     * Parse a string into an integer. (Null safe and number safe). Can raise a
     * NumberFormatException, if the number is greater than an integer.
     * 
     * @param string
     *            The input
     * @param defaultValue
     *            If the input cannot be parse, value is returned
     * @return The parsed result
     */
    public static Integer parseInt(final String string, final Integer defaultValue) {
        return parseInt(string, defaultValue, RADIX);
    }

    /**
     * Parse a string into an integer. (Null safe and number safe). Can raise a
     * NumberFormatException, if the number is greater than an integer.
     * 
     * @param string
     *            The input
     * @param defaultValue
     *            If the input cannot be parse, value is returned
     * @return The parsed result
     */
    public static Integer parseInt(final String string, final Integer defaultValue, final int radix) {
        if (NumberUtils.isNumberInteger(string)) {
            return Integer.parseInt(string, radix);
        }
        return defaultValue;
    }

    /**
     * Parse a string into a float. (Null safe and number safe). Return null, if
     * the string is null or not a number.
     * 
     * @param string
     *            The input
     * @return The parsed result
     */
    public static Float parseFloat(final String string) {
        return parseFloat(string, null);
    }

    /**
     * Parse a string into a float. (Null safe and number safe)
     * 
     * @param string
     *            The input
     * @param defaultValue
     *            If the input cannot be parse, value is returned
     * @return The parsed result
     */
    public static Float parseFloat(final String string, final Float defaultValue) {
        if (NumberUtils.isNumberDecimal(string)) {
            return Float.parseFloat(string);
        }
        return defaultValue;
    }

    /**
     * Parse a string into a long. (Null safe and number safe). Return null, if
     * the string is null or not a number.
     * 
     * @param string
     *            The input
     * @return The parsed result
     */
    public static Long parseLong(final String string) {
        return parseLong(string, null, RADIX);
    }

    /**
     * Parse a string into a long. (Null safe and number safe).
     * 
     * @param string
     *            The input
     * @param defaultValue
     *            If the input cannot be parse, value is returned
     * @return The parsed result
     */
    public static Long parseLong(final String string, final Long defaultValue) {
        return parseLong(string, defaultValue, RADIX);
    }

    /**
     * Parse a string into a long. (Null safe and number safe)
     * 
     * @param string
     *            The input
     * @param defaultValue
     *            If the input cannot be parse, value is returned
     * @return The parsed result
     */
    public static Long parseLong(final String string, final Long defaultValue, final int radix) {
        if (NumberUtils.isNumberInteger(string)) {
            return Long.parseLong(string, radix);
        }
        return defaultValue;
    }

    /**
     * Parse a string into a double. (Null safe and number safe). Return null,
     * if the string is null or not a number.
     * 
     * @param string
     *            The input
     * @return The parsed result
     */
    public static Double parseDouble(final String string) {
        return parseDouble(string, null);
    }

    /**
     * Parse a string into a double. (Null safe and number safe)
     * 
     * @param string
     *            The input
     * @param defaultValue
     *            If the input cannot be parse, value is returned
     * @return The parsed result
     */
    public static Double parseDouble(final String string, final Double defaultValue) {
        if (NumberUtils.isNumberDecimal(string)) {
            return Double.parseDouble(string);
        }
        return defaultValue;
    }

    /**
     * Parse a string into a byte. (Null safe and number safe). Return null, if
     * the string is null or not a number. Can raise a NumberFormatException, if
     * the number is greater than an integer.
     * 
     * @param string
     *            The input
     * @return The parsed result
     */
    public static Short parseShort(final String string) {
        return parseShort(string, null, RADIX);
    }

    /**
     * Parse a string into a byte. (Null safe and number safe). Can raise a
     * NumberFormatException, if the number is greater than an integer.
     * 
     * @param string
     *            The input
     * @param defaultValue
     *            If the input cannot be parse, value is returned
     * @return The parsed result
     */
    public static Short parseShort(final String string, final Short defaultValue) {
        return parseShort(string, defaultValue, RADIX);
    }

    /**
     * Parse a string into a short. (Null safe and number safe). Can raise a
     * NumberFormatException, if the number is greater than an integer.
     * 
     * @param string
     *            The input
     * @param defaultValue
     *            If the input cannot be parse, value is returned
     * @return The parsed result
     */
    public static Short parseShort(final String string, final Short defaultValue, final int radix) {
        if (NumberUtils.isNumberInteger(string)) {
            int value = Integer.parseInt(string, radix);
            if (value >= Short.MIN_VALUE && value <= Short.MAX_VALUE) {
                return (short) value;
            }
        }
        return defaultValue;
    }

    /**
     * Parse a string into a byte. (Null safe and number safe). Return null, if
     * the string is null or not a number. Can raise a NumberFormatException, if
     * the number is greater than an integer.
     * 
     * @param string
     *            The input
     * @return The parsed result
     */
    public static Byte parseByte(final String string) {
        return parseByte(string, null, RADIX);
    }

    /**
     * Parse a string into a byte. (Null safe and number safe). Return null, if
     * the string is null or not a number. Can raise a NumberFormatException, if
     * the number is greater than an integer.
     * 
     * @param string
     *            The input
     * @param defaultValue
     *            If the input cannot be parse, value is returned
     * @return The parsed result
     */
    public static Byte parseByte(final String string, final Byte defaultValue) {
        return parseByte(string, defaultValue, RADIX);
    }

    /**
     * Parse a string into a byte. (Null safe and number safe). Can raise a
     * NumberFormatException, if the number is greater than an integer.
     * 
     * @param string
     *            The input
     * 
     * @param defaultValue
     *            If the input cannot be parse, value is returned
     * @param radix
     *            The radix
     * @return The parsed result
     */
    public static Byte parseByte(final String string, final Byte defaultValue, final int radix) {
        if (NumberUtils.isNumberInteger(string)) {
            int value = Integer.parseInt(string, radix);
            if (value >= Byte.MIN_VALUE && value <= Byte.MAX_VALUE) {
                return (byte) value;
            }
        }
        return defaultValue;
    }
}