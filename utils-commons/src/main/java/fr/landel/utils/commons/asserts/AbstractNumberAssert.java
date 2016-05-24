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
package fr.landel.utils.commons.asserts;

/**
 * Assertion utility class that assists in validating arguments for numbers.
 *
 * @since 14 mai 2016
 * @author Gilles
 *
 */
public abstract class AbstractNumberAssert extends AbstractStringAssert {

    /**
     * Assert that the first number is equal to the second one.
     * 
     * <pre>
     * AssertUtils.isEqual(10, 20);
     * </pre>
     * 
     * @param number1
     *            The first number
     * @param number2
     *            The second number
     * @param <N>
     *            The type of each number <code>Byte</code>, <code>Short</code>,
     *            <code>Integer</code>, <code>Long</code>, <code>Float</code>,
     *            <code>Double</code>, <code>BigInteger</code> or
     *            <code>BigDecimal</code>. Supports new <code>Number</code>,
     *            types only if it implements <code>Comparable</code>.
     * @throws IllegalArgumentException
     *             if at least one number is {@code null} and if number are not
     *             equal.
     */
    public static <N extends Number> void isEqual(final N number1, final N number2) {
        isEqual(number1, number2, (String) null);
    }

    /**
     * Assert that the first number is equal to the second one.
     * 
     * <pre>
     * AssertUtils.isEqual(10, 20, &quot;The numbers are not equal&quot;);
     * </pre>
     * 
     * @param number1
     *            The first number
     * @param number2
     *            The second number
     * @param message
     *            the exception message, use the default assertion if null (%p
     *            or %1$p can be used to display parameter value, see
     *            explanation in the class description)
     * @param arguments
     *            the message arguments (use with String.format)
     * @param <N>
     *            The type of each number <code>Byte</code>, <code>Short</code>,
     *            <code>Integer</code>, <code>Long</code>, <code>Float</code>,
     *            <code>Double</code>, <code>BigInteger</code> or
     *            <code>BigDecimal</code>. Supports new <code>Number</code>,
     *            types only if it implements <code>Comparable</code>.
     * @throws IllegalArgumentException
     *             if at least one number is {@code null} and if number are not
     *             equal.
     */
    public static <N extends Number & Comparable<N>> void isEqual(final N number1, final N number2, final String message,
            final Object... arguments) {
        isEqual(number1, number2, null, message, arguments);
    }

    /**
     * Assert that the first number is equal to the second one.
     * 
     * <pre>
     * AssertUtils.isEqual(10, 20, exceptionToThrowOnError);
     * </pre>
     * 
     * @param number1
     *            The first number
     * @param number2
     *            The second number message the exception message, use the
     *            default assertion if null
     * @param exception
     *            the exception to throw on error
     * @param <N>
     *            The type of each number <code>Byte</code>, <code>Short</code>,
     *            <code>Integer</code>, <code>Long</code>, <code>Float</code>,
     *            <code>Double</code>, <code>BigInteger</code> or
     *            <code>BigDecimal</code>. Supports new <code>Number</code>,
     *            types only if it implements <code>Comparable</code>.
     * @param <E>
     *            The type of exception
     * @throws E
     *             if at least one number is {@code null} and if number are not
     *             equal. The standard exception is appended as suppressed.
     */
    public static <N extends Number & Comparable<N>, E extends Throwable> void isEqual(final N number1, final N number2, final E exception)
            throws E {
        isEqual(number1, number2, exception, null);
    }

    private static <N extends Number & Comparable<N>, E extends Throwable> void isEqual(final N number1, final N number2, final E exception,
            final String message, final Object... arguments) throws E {
        if (compareNumber(number1, number2, exception) != 0) {
            manageExceptions("number1 is not equal to number2.", exception, message, new Object[] {number1, number2}, arguments);
        }
    }

    /**
     * Assert that the first number is not equal to the second one.
     * 
     * <pre>
     * AssertUtils.isNotEqual(10, 20);
     * </pre>
     * 
     * @param number1
     *            The first number
     * @param number2
     *            The second number
     * @param <N>
     *            The type of each number <code>Byte</code>, <code>Short</code>,
     *            <code>Integer</code>, <code>Long</code>, <code>Float</code>,
     *            <code>Double</code>, <code>BigInteger</code> or
     *            <code>BigDecimal</code>. Supports new <code>Number</code>,
     *            types only if it implements <code>Comparable</code>.
     * @throws IllegalArgumentException
     *             if at least one number is {@code null} and if number are
     *             equal.
     */
    public static <N extends Number> void isNotEqual(final N number1, final N number2) {
        isNotEqual(number1, number2, (String) null);
    }

    /**
     * Assert that the first number is not equal to the second one.
     * 
     * <pre>
     * AssertUtils.isNotEqual(10, 20, &quot;The numbers are equal&quot;);
     * </pre>
     * 
     * @param number1
     *            The first number
     * @param number2
     *            The second number
     * @param message
     *            the exception message, use the default assertion if null (%p
     *            or %1$p can be used to display parameter value, see
     *            explanation in the class description)
     * @param arguments
     *            the message arguments (use with String.format)
     * @param <N>
     *            The type of each number <code>Byte</code>, <code>Short</code>,
     *            <code>Integer</code>, <code>Long</code>, <code>Float</code>,
     *            <code>Double</code>, <code>BigInteger</code> or
     *            <code>BigDecimal</code>. Supports new <code>Number</code>,
     *            types only if it implements <code>Comparable</code>.
     * @throws IllegalArgumentException
     *             if at least one number is {@code null} and if number are
     *             equal.
     */
    public static <N extends Number & Comparable<N>> void isNotEqual(final N number1, final N number2, final String message,
            final Object... arguments) {
        if (compareNumber(number1, number2, message, arguments) == 0) {
            throw new IllegalArgumentException(
                    getMessage("number1 is equal to number2.", message, new Object[] {number1, number2}, arguments));
        }
    }

    /**
     * Assert that the first number is not equal to the second one.
     * 
     * <pre>
     * AssertUtils.isNotEqual(10, 20, exceptionToThrowOnError);
     * </pre>
     * 
     * @param number1
     *            The first number
     * @param number2
     *            The second number message the exception message, use the
     *            default assertion if null
     * @param exception
     *            the exception to throw on error
     * @param <N>
     *            The type of each number <code>Byte</code>, <code>Short</code>,
     *            <code>Integer</code>, <code>Long</code>, <code>Float</code>,
     *            <code>Double</code>, <code>BigInteger</code> or
     *            <code>BigDecimal</code>. Supports new <code>Number</code>,
     *            types only if it implements <code>Comparable</code>.
     * @param <E>
     *            The type of exception
     * @throws E
     *             if at least one number is {@code null} and if number are
     *             equal. The standard exception is appended as suppressed.
     */
    public static <N extends Number & Comparable<N>, E extends Throwable> void isNotEqual(final N number1, final N number2,
            final E exception) throws E {
        if (compareNumber(number1, number2, exception) == 0) {
            exception.addSuppressed(new IllegalArgumentException("number1 is equal to number2."));
            throw exception;
        }
    }

    /**
     * Assert that the first number is greater than the second one.
     * 
     * <pre>
     * AssertUtils.isGT(10, 20);
     * </pre>
     * 
     * @param number1
     *            The first number
     * @param number2
     *            The second number
     * @param <N>
     *            The type of each number <code>Byte</code>, <code>Short</code>,
     *            <code>Integer</code>, <code>Long</code>, <code>Float</code>,
     *            <code>Double</code>, <code>BigInteger</code> or
     *            <code>BigDecimal</code>. Supports new <code>Number</code>,
     *            types only if it implements <code>Comparable</code>.
     * @throws IllegalArgumentException
     *             If at least one number is {@code null} and if number1 is not
     *             greater than number2.
     */
    public static <N extends Number & Comparable<N>> void isGT(final N number1, final N number2) {
        AbstractNumberAssert.isGT(number1, number2, (String) null);
    }

    /**
     * Assert that the first number is greater than the second one. In message,
     * parameters can be used through format '%p' (first %p will be replaced by
     * first parameter, second...).
     * 
     * 
     * <pre>
     * AssertUtils.isGT(10, 20, &quot;The number1 is not greater than number2&quot;);
     * </pre>
     * 
     * @param number1
     *            The first number
     * @param number2
     *            The second number
     * @param message
     *            the exception message, use the default assertion if null (%p
     *            or %1$p can be used to display parameter value, see
     *            explanation in the class description)
     * @param arguments
     *            the message arguments (use with String.format)
     * @param <N>
     *            The type of each number <code>Byte</code>, <code>Short</code>,
     *            <code>Integer</code>, <code>Long</code>, <code>Float</code>,
     *            <code>Double</code>, <code>BigInteger</code> or
     *            <code>BigDecimal</code>. Supports new <code>Number</code>,
     *            types only if it implements <code>Comparable</code>.
     * @throws IllegalArgumentException
     *             If at least one number is {@code null} and if number is not
     *             greater than number2
     */
    public static <N extends Number & Comparable<N>> void isGT(final N number1, final N number2, final String message,
            final Object... arguments) {
        if (compareNumber(number1, number2, message, arguments) <= 0) {
            throw new IllegalArgumentException(
                    getMessage("number1 is not greater than number2.", message, new Object[] {number1, number2}, arguments));
        }
    }

    /**
     * Assert that the first number is greater than the second one.
     * 
     * <pre>
     * AssertUtils.isGT(10, 20, exceptionToThrowOnError);
     * </pre>
     * 
     * @param number1
     *            The first number
     * @param number2
     *            The second number
     * @param exception
     *            the exception to throw on error
     * @param <N>
     *            The type of each number <code>Byte</code>, <code>Short</code>,
     *            <code>Integer</code>, <code>Long</code>, <code>Float</code>,
     *            <code>Double</code>, <code>BigInteger</code> or
     *            <code>BigDecimal</code>. Supports new <code>Number</code>,
     *            types only if it implements <code>Comparable</code>.
     * @param <E>
     *            The type of exception
     * @throws E
     *             If at least one number is {@code null} and if number is not
     *             greater than number2. The standard exception is appended as
     *             suppressed.
     */
    public static <N extends Number & Comparable<N>, E extends Throwable> void isGT(final N number1, final N number2, final E exception)
            throws E {
        if (compareNumber(number1, number2, exception) <= 0) {
            exception.addSuppressed(new IllegalArgumentException("number1 is not greater than number2."));
            throw exception;
        }
    }

    /**
     * Assert that the first number is greater than or equal to the second one.
     * 
     * @param number1
     *            The first number
     * @param number2
     *            The second number
     * @param <N>
     *            The type of each number <code>Byte</code>, <code>Short</code>,
     *            <code>Integer</code>, <code>Long</code>, <code>Float</code>,
     *            <code>Double</code>, <code>BigInteger</code> or
     *            <code>BigDecimal</code>. Supports new <code>Number</code>,
     *            types only if it implements <code>Comparable</code>.
     * @throws IllegalArgumentException
     *             if at least one number is {@code null} and if number1 is
     *             lower than number2.
     */
    public static <N extends Number & Comparable<N>> void isGTE(final N number1, final N number2) {
        AbstractNumberAssert.isGTE(number1, number2, (String) null);
    }

    /**
     * Assert that the first number is greater than or equal to the second one.
     * 
     * @param number1
     *            The first number
     * @param number2
     *            The second number
     * @param message
     *            the exception message, use the default assertion if null (%p
     *            or %1$p can be used to display parameter value, see
     *            explanation in the class description)
     * @param arguments
     *            the message arguments (use with String.format)
     * @param <N>
     *            The type of each number <code>Byte</code>, <code>Short</code>,
     *            <code>Integer</code>, <code>Long</code>, <code>Float</code>,
     *            <code>Double</code>, <code>BigInteger</code> or
     *            <code>BigDecimal</code>. Supports new <code>Number</code>,
     *            types only if it implements <code>Comparable</code>.
     * @throws IllegalArgumentException
     *             if at least one number is {@code null} and if number1 is
     *             lower than number2.
     */
    public static <N extends Number & Comparable<N>> void isGTE(final N number1, final N number2, final String message,
            final Object... arguments) {
        if (compareNumber(number1, number2, message, arguments) < 0) {
            throw new IllegalArgumentException(
                    getMessage("number1 is not greater than or equal to number2.", message, new Object[] {number1, number2}, arguments));
        }
    }

    /**
     * Assert that the first number is greater than or equal to the second one.
     * 
     * @param number1
     *            The first number
     * @param number2
     *            The second number
     * @param exception
     *            the exception to throw on error
     * @param <N>
     *            The type of each number <code>Byte</code>, <code>Short</code>,
     *            <code>Integer</code>, <code>Long</code>, <code>Float</code>,
     *            <code>Double</code>, <code>BigInteger</code> or
     *            <code>BigDecimal</code>. Supports new <code>Number</code>,
     *            types only if it implements <code>Comparable</code>.
     * @param <E>
     *            The type of exception
     * @throws E
     *             if at least one number is {@code null} and if number1 is
     *             lower than number2. The standard exception is appended as
     *             suppressed.
     */
    public static <N extends Number & Comparable<N>, E extends Throwable> void isGTE(final N number1, final N number2, final E exception)
            throws E {
        if (compareNumber(number1, number2, exception) < 0) {
            exception.addSuppressed(new IllegalArgumentException("number1 is not greater than or equal to number2."));
            throw exception;
        }
    }

    /**
     * Assert that the first number is lower than the second one.
     * 
     * @param number1
     *            The first number
     * @param number2
     *            The second number
     * @param <N>
     *            The type of each number <code>Byte</code>, <code>Short</code>,
     *            <code>Integer</code>, <code>Long</code>, <code>Float</code>,
     *            <code>Double</code>, <code>BigInteger</code> or
     *            <code>BigDecimal</code>. Supports new <code>Number</code>,
     *            types only if it implements <code>Comparable</code>.
     * @throws IllegalArgumentException
     *             if at least one number is {@code null} and if number1 is not
     *             lower than number2.
     */
    public static <N extends Number & Comparable<N>> void isLT(final N number1, final N number2) {
        AbstractNumberAssert.isLT(number1, number2, (String) null);
    }

    /**
     * Assert that the first number is lower than the second one.
     * 
     * @param number1
     *            The first number
     * @param number2
     *            The second number
     * @param message
     *            the exception message, use the default assertion if null (%p
     *            or %1$p can be used to display parameter value, see
     *            explanation in the class description)
     * @param arguments
     *            the message arguments (use with String.format)
     * @param <N>
     *            The type of each number <code>Byte</code>, <code>Short</code>,
     *            <code>Integer</code>, <code>Long</code>, <code>Float</code>,
     *            <code>Double</code>, <code>BigInteger</code> or
     *            <code>BigDecimal</code>. Supports new <code>Number</code>,
     *            types only if it implements <code>Comparable</code>.
     * @throws IllegalArgumentException
     *             if at least one number is {@code null} and if number1 is not
     *             lower than number2.
     */
    public static <N extends Number & Comparable<N>> void isLT(final N number1, final N number2, final String message,
            final Object... arguments) {
        if (compareNumber(number1, number2, message, arguments) >= 0) {
            throw new IllegalArgumentException(
                    getMessage("number1 is not lower than number2.", message, new Object[] {number1, number2}, arguments));
        }
    }

    /**
     * Assert that the first number is lower than the second one.
     * 
     * @param number1
     *            The first number
     * @param number2
     *            The second number
     * @param exception
     *            the exception to throw on error
     * @param <N>
     *            The type of each number <code>Byte</code>, <code>Short</code>,
     *            <code>Integer</code>, <code>Long</code>, <code>Float</code>,
     *            <code>Double</code>, <code>BigInteger</code> or
     *            <code>BigDecimal</code>. Supports new <code>Number</code>,
     *            types only if it implements <code>Comparable</code>.
     * @param <E>
     *            The type of exception
     * @throws E
     *             if at least one number is {@code null} and if number1 is not
     *             lower than number2. The standard exception is appended as
     *             suppressed.
     */
    public static <N extends Number & Comparable<N>, E extends Throwable> void isLT(final N number1, final N number2, final E exception)
            throws E {
        if (compareNumber(number1, number2, exception) >= 0) {
            exception.addSuppressed(new IllegalArgumentException("number1 is not not lower than number2."));
            throw exception;
        }
    }

    /**
     * Assert that the first number is lower than or equal to the second one.
     * 
     * @param number1
     *            The first number
     * @param number2
     *            The second number
     * @param <N>
     *            The type of each number <code>Byte</code>, <code>Short</code>,
     *            <code>Integer</code>, <code>Long</code>, <code>Float</code>,
     *            <code>Double</code>, <code>BigInteger</code> or
     *            <code>BigDecimal</code>. Supports new <code>Number</code>,
     *            types only if it implements <code>Comparable</code>.
     * @throws IllegalArgumentException
     *             if at least one number is {@code null} and if number1 is
     *             greater than number2.
     */
    public static <N extends Number & Comparable<N>> void isLTE(final N number1, final N number2) {
        AbstractNumberAssert.isLTE(number1, number2, (String) null);
    }

    /**
     * Assert that the first number is lower than or equal to the second one.
     * 
     * @param number1
     *            The first number
     * @param number2
     *            The second number
     * @param message
     *            the exception message, use the default assertion if null (%p
     *            or %1$p can be used to display parameter value, see
     *            explanation in the class description)
     * @param arguments
     *            the message arguments (use with String.format)
     * @param <N>
     *            The type of each number <code>Byte</code>, <code>Short</code>,
     *            <code>Integer</code>, <code>Long</code>, <code>Float</code>,
     *            <code>Double</code>, <code>BigInteger</code> or
     *            <code>BigDecimal</code>. Supports new <code>Number</code>,
     *            types only if it implements <code>Comparable</code>.
     * @throws IllegalArgumentException
     *             if at least one number is {@code null} and if number1 is
     *             greater than number2.
     */
    public static <N extends Number & Comparable<N>> void isLTE(final N number1, final N number2, final String message,
            final Object... arguments) {
        if (compareNumber(number1, number2, message, arguments) > 0) {
            throw new IllegalArgumentException(
                    getMessage("number1 is not lower than or equal to number2.", message, new Object[] {number1, number2}, arguments));
        }
    }

    /**
     * Assert that the first number is lower than or equal to the second one.
     * 
     * @param number1
     *            The first number
     * @param number2
     *            The second number
     * @param exception
     *            the exception to throw on error
     * @param <N>
     *            The type of each number <code>Byte</code>, <code>Short</code>,
     *            <code>Integer</code>, <code>Long</code>, <code>Float</code>,
     *            <code>Double</code>, <code>BigInteger</code> or
     *            <code>BigDecimal</code>. Supports new <code>Number</code>,
     *            types only if it implements <code>Comparable</code>.
     * @param <E>
     *            The type of exception
     * @throws E
     *             if at least one number is {@code null} and if number1 is
     *             greater than number2. The standard exception is appended as
     *             suppressed.
     */
    public static <N extends Number & Comparable<N>, E extends Throwable> void isLTE(final N number1, final N number2, final E exception)
            throws E {
        if (compareNumber(number1, number2, exception) > 0) {
            exception.addSuppressed(new IllegalArgumentException("number1 is not lower than or equal to number2."));
            throw exception;
        }
    }

    private static <N extends Number & Comparable<N>> int compareNumber(final N number1, final N number2, final String message,
            final Object... arguments) {
        if (number1 != null && number2 != null) {
            return number1.compareTo(number2);
        } else if (number1 != null) {
            throw new IllegalArgumentException(getMessage("number1 is null.", message, new Object[] {number1, number2}, arguments));
        } else if (number2 != null) {
            throw new IllegalArgumentException(getMessage("number2 is null.", message, new Object[] {number1, number2}, arguments));
        } else {
            throw new IllegalArgumentException(
                    getMessage("number1 and number2 are null.", message, new Object[] {number1, number2}, arguments));
        }
    }

    private static <N extends Number & Comparable<N>, E extends Throwable> int compareNumber(final N number1, final N number2,
            final E exception) throws E {
        if (number1 != null && number2 != null) {
            return number1.compareTo(number2);
        } else if (number1 != null) {
            exception.addSuppressed(new IllegalArgumentException("number1 is null."));
        } else if (number2 != null) {
            exception.addSuppressed(new IllegalArgumentException("number2 is null."));
        } else {
            exception.addSuppressed(new IllegalArgumentException("number1 and number2 are null."));
        }
        throw exception;
    }
}
