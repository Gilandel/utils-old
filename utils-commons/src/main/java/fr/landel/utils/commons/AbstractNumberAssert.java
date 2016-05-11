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

/**
 * Assertion utility class that assists in validating arguments.
 *
 * <p>
 * Useful for identifying programmer errors early and clearly at runtime.
 *
 * <p>
 * For example, if the contract of a public method states it does not allow
 * {@code null} arguments, {@code Assert} can be used to validate that contract.
 * Doing this clearly indicates a contract violation when it occurs and protects
 * the class's invariants.
 *
 * <p>
 * Typically used to validate method arguments rather than configuration
 * properties, to check for cases that are usually programmer errors rather than
 * configuration errors. In contrast to configuration initialization code, there
 * is usually no point in falling back to defaults in such methods.
 *
 * <p>
 * This class is similar to JUnit's assertion library. If an argument value is
 * deemed invalid, an {@link IllegalArgumentException} is thrown (typically).
 * For example:
 *
 * <pre>
 * Assert.notNull(clazz, &quot;The class must not be null&quot;);
 * Assert.isTrue(i &gt; 0, &quot;The value must be greater than zero&quot;);
 * </pre>
 *
 * <p>
 * Mainly for internal use within the framework; consider
 * <a href="http://commons.apache.org/proper/commons-lang/">Apache's Commons
 * Lang</a> for a more comprehensive suite of {@code String} utilities.
 * 
 * <p>
 * To display parameters in exception messages, use %p or %1$p
 * </p>
 * 
 * <pre>
 * Assert.isGT(10, 20, &quot;The number '%p' is not greater than number '%p'&quot;);
 * // Exception: "The number '10' is not greater than number '20'"
 * Assert.isGT(10, 20, &quot;'%2$p' &gt; '%1$p'&quot;);
 * // Exception: "'20' &gt; '10'"
 * </pre>
 *
 * @see <a href=
 *      "http://docs.spring.io/spring/docs/2.0.x/api/org/springframework/util/Assert.html?is-external=true">
 *      org.springframework.util.Assert</a>
 *
 * @author Keith Donald
 * @author Juergen Hoeller
 * @author Colin Sampaleanu
 * @author Rob Harrop
 * @author Sam Brannen
 * @author Gilles Landel
 * @since 1.1.2
 */
public abstract class AbstractNumberAssert extends AbstractStringAssert {

    /**
     * Assert that the first number is equal to the second one.
     * 
     * <pre>
     * Assert.isEqual(10, 20);
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
        AbstractNumberAssert.isEqual(number1, number2, (String) null);
    }

    /**
     * Assert that the first number is equal to the second one.
     * 
     * <pre>
     * Assert.isEqual(10, 20, &quot;The numbers are not equal&quot;);
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
        if (compareNumber(number1, number2, message, arguments) == 0) {
            throw new IllegalArgumentException(
                    getMessage("number1 is not equal to number2.", message, new Object[] {number1, number2}, arguments));
        }
    }

    /**
     * Assert that the first number is equal to the second one.
     * 
     * <pre>
     * Assert.isEqual(10, 20, exceptionToThrowOnError);
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
        if (compareNumber(number1, number2, exception) == 0) {
            exception.addSuppressed(new IllegalArgumentException("number1 is not equal to number2."));
            throw exception;
        }
    }

    /**
     * Assert that the first number is greater than the second one.
     * 
     * <pre>
     * Assert.isGT(10, 20);
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
     * Assert.isGT(10, 20, &quot;The number1 is not greater than number2&quot;);
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
     * Assert.isGT(10, 20, exceptionToThrowOnError);
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
