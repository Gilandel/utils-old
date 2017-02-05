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

import java.math.BigDecimal;
import java.math.BigInteger;
import java.nio.charset.StandardCharsets;
import java.util.Arrays;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.concurrent.atomic.AtomicLong;

import org.apache.commons.lang3.StringUtils;

/**
 * Utility class to manage numbers.
 *
 * @since Nov 27, 2015
 * @author Gilles Landel
 *
 */
public final class NumberUtils extends org.apache.commons.lang3.math.NumberUtils {

    private static final int RADIX = 10;
    private static final int TEN = 10;

    /**
     * Hidden constructor.
     */
    private NumberUtils() {
        super();
    }

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

    /**
     * Check if the string contains an integer number ({@link Byte},
     * {@link Short}, {@link Integer}, {@link Long} or {@link BigInteger}). The
     * following regular expression is applied {@code [+-]?\d+}
     * 
     * <p>
     * Only format is checked, not if the {@code float} is an {@code int} is a
     * {@code long} ({@link Integer#MAX_VALUE} or {@link Integer#MIN_VALUE})...
     * </p>
     * 
     * <pre>
     * NumberUtils.isNumberInteger("25"); // -&gt; true
     * NumberUtils.isNumberInteger("+25"); // -&gt; true
     * NumberUtils.isNumberInteger("-25"); // -&gt; true
     * NumberUtils.isNumberInteger("+25l"); // -&gt; false
     * NumberUtils.isNumberInteger("-25L"); // -&gt; false
     * NumberUtils.isNumberInteger("25d"); // -&gt; false
     * NumberUtils.isNumberInteger("25f"); // -&gt; false
     * 
     * NumberUtils.isNumberInteger(""); // -&gt; false
     * NumberUtils.isNumberInteger("text"); // -&gt; false
     * NumberUtils.isNumberInteger((String) null); // -&gt; false
     * </pre>
     * 
     * 
     * @param string
     *            the input String
     * @return true, if integer number
     */
    public static boolean isNumberInteger(final String string) {
        return isNumberInteger(string, false);
    }

    /**
     * Check if the string contains an integer number ({@link Byte},
     * {@link Short}, {@link Integer}, {@link Long} or {@link BigInteger}). If
     * typeSupported is true, the following regular expression is applied
     * {@code [+-]?\d+[lL]?}, otherwise {@code [+-]?\d+}
     * 
     * <p>
     * Only format is checked, not if the {@code float} is an {@code int} is a
     * {@code long} ({@link Integer#MAX_VALUE} or {@link Integer#MIN_VALUE})...
     * </p>
     * 
     * <pre>
     * NumberUtils.isNumberInteger("25", false); // -&gt; true
     * NumberUtils.isNumberInteger("+25", false); // -&gt; true
     * NumberUtils.isNumberInteger("-25", false); // -&gt; true
     * NumberUtils.isNumberInteger("+25l", false); // -&gt; false
     * NumberUtils.isNumberInteger("-25L", false); // -&gt; false
     * NumberUtils.isNumberInteger("+25l", true); // -&gt; true
     * NumberUtils.isNumberInteger("-25L", true); // -&gt; true
     * NumberUtils.isNumberInteger("25d", true); // -&gt; false
     * NumberUtils.isNumberInteger("25f", true); // -&gt; false
     * 
     * NumberUtils.isNumberInteger("", true); // -&gt; false
     * NumberUtils.isNumberInteger("text", true); // -&gt; false
     * NumberUtils.isNumberInteger((String) null, true); // -&gt; false
     * </pre>
     * 
     * 
     * @param string
     *            the input String
     * @param typeSupported
     *            if typed is supported (like: "10L")
     * @return true, if integer number
     */
    public static boolean isNumberInteger(final String string, final boolean typeSupported) {
        if (StringUtils.isEmpty(string)) {
            return false;
        }
        int start = 0;
        if (string.startsWith("-") || string.startsWith("+")) {
            start = 1;
        }
        int length = string.length();
        short nb = 0;
        byte typed = 0;

        byte[] bytes = string.getBytes(StandardCharsets.UTF_8);

        for (int i = start; i < bytes.length; i++) {
            if (Character.isDigit(bytes[i])) {
                nb++;
            } else if (typeSupported && i == length - 1) {
                typed = bytes[i] == 'l' || bytes[i] == 'L' ? (byte) 1 : 0;
            }
        }
        return start + nb + typed == bytes.length;
    }

    /**
     * Check if the class is an integer number class ({@link Byte},
     * {@link Short}, {@link Integer}, {@link Long}, {@link BigInteger},
     * {@link AtomicInteger} or {@link AtomicLong})
     * 
     * @param clazz
     *            the input class
     * @return true, if integer number
     */
    public static boolean isNumberInteger(final Class<?> clazz) {
        boolean result = false;
        if (clazz != null) {
            if (Integer.class.isAssignableFrom(clazz)) {
                result = true;
            } else if (Long.class.isAssignableFrom(clazz)) {
                result = true;
            } else if (Byte.class.isAssignableFrom(clazz)) {
                result = true;
            } else if (Short.class.isAssignableFrom(clazz)) {
                result = true;
            } else if (BigInteger.class.isAssignableFrom(clazz)) {
                result = true;
            } else if (AtomicInteger.class.isAssignableFrom(clazz)) {
                result = true;
            } else if (AtomicLong.class.isAssignableFrom(clazz)) {
                result = true;
            }
        }
        return result;
    }

    /**
     * Check if the number is an integer number ({@link Byte}, {@link Short},
     * {@link Integer}, {@link Long} or {@link BigInteger})
     * 
     * @param object
     *            the input object
     * @return true, if integer number
     */
    public static boolean isNumberInteger(final Object object) {
        return isNumberInteger(ClassUtils.getClass(object));
    }

    /**
     * Check if the string contains a decimal number ({@link Float},
     * {@link Double}, {@link BigDecimal}). The following regular expression is
     * applied {@code [+-]?(\\d+)?\\.\\d+}
     * 
     * <p>
     * Only format is checked, not if the {@code float} is a {@code double}
     * ({@link Float#MAX_VALUE} or {@link Float#MIN_VALUE}) or an {@code int} is
     * a {@code long}...
     * </p>
     * 
     * <pre>
     * NumberUtils.isNumberDecimal("25.6"); // -&gt; true
     * NumberUtils.isNumberDecimal("+25.6"); // -&gt; true
     * NumberUtils.isNumberDecimal("-25.6"); // -&gt; true
     * NumberUtils.isNumberDecimal("+25.6d"); // -&gt; false
     * NumberUtils.isNumberDecimal("-25.6F"); // -&gt; false
     * NumberUtils.isNumberDecimal("25d"); // -&gt; false
     * NumberUtils.isNumberDecimal("25f"); // -&gt; false
     * NumberUtils.isNumberDecimal(".25"); // -&gt; true
     * NumberUtils.isNumberDecimal("25."); // -&gt; false
     * NumberUtils.isNumberDecimal("25"); // -&gt; false
     * 
     * NumberUtils.isNumberDecimal(""); // -&gt; false
     * NumberUtils.isNumberDecimal("text"); // -&gt; false
     * NumberUtils.isNumberDecimal((String) null); // -&gt; false
     * </pre>
     * 
     * @param string
     *            the input String
     * @return true, if decimal number
     */
    public static boolean isNumberDecimal(final String string) {
        return isNumberDecimal(string, false);
    }

    /**
     * Check if the string contains a decimal number ({@link Float},
     * {@link Double}, {@link BigDecimal}). if typeSupported is true, the
     * following regular expression is applied
     * {@code [+-]?(\\d+[dfDF]|(\\d+)?\\.\\d+[dfDF]?)}, otherwise
     * {@code [+-]?(\\d+)?\\.\\d+}
     * 
     * <p>
     * Only format is checked, not if the {@code float} is a {@code double}
     * ({@link Float#MAX_VALUE} or {@link Float#MIN_VALUE}) or an {@code int} is
     * a {@code long}...
     * </p>
     * 
     * <pre>
     * NumberUtils.isNumberDecimal("25.6"); // -&gt; true
     * NumberUtils.isNumberDecimal("+25.6"); // -&gt; true
     * NumberUtils.isNumberDecimal("-25.6"); // -&gt; true
     * NumberUtils.isNumberDecimal("+25.6d"); // -&gt; false
     * NumberUtils.isNumberDecimal("-25.6F"); // -&gt; false
     * NumberUtils.isNumberDecimal("25d"); // -&gt; false
     * NumberUtils.isNumberDecimal("25f"); // -&gt; false
     * NumberUtils.isNumberDecimal(".25"); // -&gt; true
     * NumberUtils.isNumberDecimal("25."); // -&gt; false
     * NumberUtils.isNumberDecimal("25"); // -&gt; false
     * 
     * NumberUtils.isNumberDecimal(""); // -&gt; false
     * NumberUtils.isNumberDecimal("text"); // -&gt; false
     * NumberUtils.isNumberDecimal((String) null); // -&gt; false
     * </pre>
     * 
     * @param string
     *            the input String
     * @param typeSupported
     *            if typed is supported (like: "10.2f")
     * @return true, if decimal number
     */
    public static boolean isNumberDecimal(final String string, final boolean typeSupported) {
        return isNumberDecimal(string, typeSupported, false);
    }

    /**
     * Check if the string contains a decimal number ({@link Float},
     * {@link Double}, {@link BigDecimal}).
     * 
     * <p>
     * Regular expression used by cases:
     * </p>
     * 
     * <ul>
     * <li>typeSupported is true and lenient is false:
     * {@code [+-]?(\\d+[dfDF]|(\\d+)?\\.\\d+[dfDF]?)}</li>
     * 
     * <li>typeSupported is true and lenient is true:
     * {@code [+-]?(\\d+|(\\d+)?\\.\\d+)[dfDF]?}</li>
     * 
     * <li>typeSupported is false and lenient is false:
     * {@code [+-]?(\\d+)?\\.\\d+}</li>
     * 
     * <li>typeSupported is false and lenient is true:
     * {@code [+-]?(\\d+|(\\d+)?\\.\\d+)?}</li>
     * </ul>
     * 
     * <p>
     * Only format is checked, not if the {@code float} is a {@code double}
     * ({@link Float#MAX_VALUE} or {@link Float#MIN_VALUE}) or an {@code int} is
     * a {@code long}...
     * </p>
     * 
     * <pre>
     * NumberUtils.isNumberDecimal("25.6", false, false); // -&gt; true
     * NumberUtils.isNumberDecimal("+25.6", false, false); // -&gt; true
     * NumberUtils.isNumberDecimal("-25.6", false, false); // -&gt; true
     * NumberUtils.isNumberDecimal("+25.6d", true, false); // -&gt; true
     * NumberUtils.isNumberDecimal("-25.6F", true, false); // -&gt; true
     * NumberUtils.isNumberDecimal("+25.6d", true, false); // -&gt; true
     * NumberUtils.isNumberDecimal("-25.6F", true, false); // -&gt; true
     * NumberUtils.isNumberDecimal(".25d", true, false); // -&gt; true
     * NumberUtils.isNumberDecimal("25d", true, false); // -&gt; true
     * NumberUtils.isNumberDecimal("25f", true, false); // -&gt; true
     * NumberUtils.isNumberDecimal("25", false, false); // -&gt; false
     * NumberUtils.isNumberDecimal("25", false, true); // -&gt; true
     * 
     * NumberUtils.isNumberDecimal("", false, true); // -&gt; false
     * NumberUtils.isNumberDecimal("text", false, true); // -&gt; false
     * NumberUtils.isNumberDecimal((String) null, false, true); // -&gt; false
     * </pre>
     * 
     * @param string
     *            the input String
     * @param typeSupported
     *            if typed is supported (like: "10.2f")
     * @param lenient
     *            set to false if dot is required
     * @return true, if decimal number
     */
    public static boolean isNumberDecimal(final String string, final boolean typeSupported, final boolean lenient) {
        if (StringUtils.isEmpty(string)) {
            return false;
        }
        int start = 0;
        if (string.startsWith("-") || string.startsWith("+")) {
            start = 1;
        }
        byte dot = 0;
        byte typed = 0;
        short nb1 = 0;
        short nb2 = 0;

        byte[] bytes = string.getBytes(StandardCharsets.UTF_8);
        byte[] types = new byte[] {'D', 'F', 'd', 'f'};

        for (int i = start; i < bytes.length; i++) {
            if (Character.isDigit(bytes[i])) {
                if (dot == 1) {
                    nb2++;
                } else {
                    nb1++;
                }
            } else if ('.' == bytes[i]) {
                if (dot == 1) {
                    return false; // multiple dots
                } else {
                    dot = 1;
                }
            } else if (typeSupported && i == bytes.length - 1 && Arrays.binarySearch(types, bytes[i]) > -1) {
                typed = (byte) 1;
            }
        }
        return compareLength(start, nb1, dot, nb2, typed, bytes, lenient);
    }

    private static boolean compareLength(final int start, final short nb1, final byte dot, final short nb2, final byte typed,
            final byte[] bytes, final boolean lenient) {
        if (nb1 > 0) {
            if (dot == 1 && nb2 > 0) {
                return start + nb1 + dot + nb2 + typed == bytes.length;
            } else if (lenient || typed == 1) {
                return start + nb1 + typed == bytes.length;
            }
        } else if (dot == 1 && nb2 > 0) {
            return start + dot + nb2 + typed == bytes.length;
        }
        return false;
    }

    /**
     * Check if the class is a decimal number class ({@link Float},
     * {@link Double}, {@link BigDecimal})
     * 
     * @param clazz
     *            the input class
     * @return true, if integer number
     */
    public static boolean isNumberDecimal(final Class<?> clazz) {
        boolean result = false;
        if (clazz != null) {
            if (Float.class.isAssignableFrom(clazz)) {
                result = true;
            } else if (Double.class.isAssignableFrom(clazz)) {
                result = true;
            } else if (BigDecimal.class.isAssignableFrom(clazz)) {
                result = true;
            }
        }
        return result;
    }

    /**
     * Check if the number is a decimal number ({@link Float}, {@link Double},
     * {@link BigDecimal})
     * 
     * @param object
     *            the input object
     * @return true, if integer number
     */
    public static boolean isNumberDecimal(final Object object) {
        return isNumberDecimal(ClassUtils.getClass(object));
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
     * @param defaultValue
     *            If the input cannot be parse, value is returned
     * @param radix
     *            The radix to be used while parsing the string
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
     * @param radix
     *            The radix to be used while parsing the string
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
     * @param radix
     *            The radix to be used while parsing the string
     * @return The parsed result
     */
    public static Integer parseInt(final String string, final Integer defaultValue, final int radix) {
        if (NumberUtils.isNumberInteger(string)) {
            return Integer.parseInt(string, radix);
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
     * @param radix
     *            The radix to be used while parsing the string
     * @return The parsed result
     */
    public static Long parseLong(final String string, final Long defaultValue, final int radix) {
        if (NumberUtils.isNumberInteger(string)) {
            return Long.parseLong(string, radix);
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
        if (NumberUtils.isNumberDecimal(string, true, true)) {
            return Float.parseFloat(string);
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
        if (NumberUtils.isNumberDecimal(string, true, true)) {
            return Double.parseDouble(string);
        }
        return defaultValue;
    }

    /**
     * Check if the number is a {@link Byte}
     * 
     * @param number
     *            The number to check
     * @param <N>
     *            The type of the number
     * @return true, if matches
     */
    public static <N extends Number> boolean isByte(final N number) {
        return isNumberType(number, Byte.class);
    }

    /**
     * Check if the number is a {@link Short}
     * 
     * @param number
     *            The number to check
     * @param <N>
     *            The type of the number
     * @return true, if matches
     */
    public static <N extends Number> boolean isShort(final N number) {
        return isNumberType(number, Short.class);
    }

    /**
     * Check if the number is an {@link Integer}
     * 
     * @param number
     *            The number to check
     * @param <N>
     *            The type of the number
     * @return true, if matches
     */
    public static <N extends Number> boolean isInteger(final N number) {
        return isNumberType(number, Integer.class);
    }

    /**
     * Check if the number is a {@link Long}
     * 
     * @param number
     *            The number to check
     * @param <N>
     *            The type of the number
     * @return true, if matches
     */
    public static <N extends Number> boolean isLong(final N number) {
        return isNumberType(number, Long.class);
    }

    /**
     * Check if the number is a {@link Float}
     * 
     * @param number
     *            The number to check
     * @param <N>
     *            The type of the number
     * @return true, if matches
     */
    public static <N extends Number> boolean isFloat(final N number) {
        return isNumberType(number, Float.class);
    }

    /**
     * Check if the number is a {@link Double}
     * 
     * @param number
     *            The number to check
     * @param <N>
     *            The type of the number
     * @return true, if matches
     */
    public static <N extends Number> boolean isDouble(final N number) {
        return isNumberType(number, Double.class);
    }

    /**
     * Check if the number is a {@link BigInteger}
     * 
     * @param number
     *            The number to check
     * @param <N>
     *            The type of the number
     * @return true, if matches
     */
    public static <N extends Number> boolean isBigInteger(final N number) {
        return isNumberType(number, BigInteger.class);
    }

    /**
     * Check if the number is a {@link BigDecimal}
     * 
     * @param number
     *            The number to check
     * @param <N>
     *            The type of the number
     * @return true, if matches
     */
    public static <N extends Number> boolean isBigDecimal(final N number) {
        return isNumberType(number, BigDecimal.class);
    }

    /**
     * Check if the number is an {@link AtomicInteger}
     * 
     * @param number
     *            The number to check
     * @param <N>
     *            The type of the number
     * @return true, if matches
     */
    public static <N extends Number> boolean isAtomicInteger(final N number) {
        return isNumberType(number, AtomicInteger.class);
    }

    /**
     * Check if the number is an {@link AtomicLong}
     * 
     * @param number
     *            The number to check
     * @param <N>
     *            The type of the number
     * @return true, if matches
     */
    public static <N extends Number> boolean isAtomicLong(final N number) {
        return isNumberType(number, AtomicLong.class);
    }

    private static <N extends Number> boolean isNumberType(final N number, final Class<? extends Number> classNumber) {
        return number != null && classNumber.isAssignableFrom(number.getClass());
    }

    /**
     * Get the sign of the number
     * 
     * @param number
     *            the number to check
     * @param <N>
     *            The type of the number
     * @return 1 if number &gt; 0, -1 if number &lt; 0 and 0 otherwise, even if
     *         null
     */
    public static <N extends Number> int signum(final N number) {
        int result = 0;
        if (number != null) {
            if (NumberUtils.isInteger(number)) {
                final Integer n = (Integer) number;
                result = n > 0 ? 1 : n < 0 ? -1 : 0;
            } else if (NumberUtils.isLong(number)) {
                final Long n = (Long) number;
                result = n > 0 ? 1 : n < 0 ? -1 : 0;
            } else if (NumberUtils.isFloat(number)) {
                final Float n = (Float) number;
                result = n > 0 ? 1 : n < 0 ? -1 : 0;
            } else if (NumberUtils.isDouble(number)) {
                final Double n = (Double) number;
                result = n > 0 ? 1 : n < 0 ? -1 : 0;
            } else if (NumberUtils.isByte(number)) {
                final Byte n = (Byte) number;
                result = n > 0 ? 1 : n < 0 ? -1 : 0;
            } else if (NumberUtils.isShort(number)) {
                final Short n = (Short) number;
                result = n > 0 ? 1 : n < 0 ? -1 : 0;
            } else if (NumberUtils.isBigInteger(number)) {
                result = ((BigInteger) number).signum();
            } else if (NumberUtils.isAtomicInteger(number)) {
                final int n = ((AtomicInteger) number).get();
                result = n > 0 ? 1 : n < 0 ? -1 : 0;
            } else if (NumberUtils.isAtomicLong(number)) {
                final long n = ((AtomicLong) number).get();
                result = n > 0 ? 1 : n < 0 ? -1 : 0;
            } else if (NumberUtils.isBigDecimal(number)) {
                result = ((BigDecimal) number).signum();
            }
        }
        return result;
    }

    /**
     * Check if the number is equal to zero
     * 
     * @param number
     *            the number to check
     * @param <N>
     *            The type of the number
     * @return true if number = 0, false otherwise
     */
    public static <N extends Number> boolean isZero(final N number) {
        boolean result = false;
        if (number != null) {
            if (NumberUtils.isInteger(number)) {
                final Integer n = (Integer) number;
                result = n == 0;
            } else if (NumberUtils.isLong(number)) {
                final Long n = (Long) number;
                result = n == 0;
            } else if (NumberUtils.isFloat(number)) {
                final Float n = (Float) number;
                result = n == 0;
            } else if (NumberUtils.isDouble(number)) {
                final Double n = (Double) number;
                result = n == 0;
            } else if (NumberUtils.isByte(number)) {
                final Byte n = (Byte) number;
                result = n == 0;
            } else if (NumberUtils.isShort(number)) {
                final Short n = (Short) number;
                result = n == 0;
            } else if (NumberUtils.isBigInteger(number)) {
                result = ((BigInteger) number).signum() == 0;
            } else if (NumberUtils.isAtomicInteger(number)) {
                final int n = ((AtomicInteger) number).get();
                result = n == 0;
            } else if (NumberUtils.isAtomicLong(number)) {
                final long n = ((AtomicLong) number).get();
                result = n == 0;
            } else if (NumberUtils.isBigDecimal(number)) {
                result = ((BigDecimal) number).signum() == 0;
            }
        }
        return result;
    }
}
