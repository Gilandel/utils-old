/*
 * #%L
 * utils-asserts
 * %%
 * Copyright (C) 2016 Gilandel
 * %%
 * Authors: Gilles Landel
 * URL: https://github.com/Gilandel
 * 
 * This file is under Apache License, version 2.0 (2004).
 * #L%
 */
package fr.landel.utils.asserts;

/**
 * Assertion utility class that assists in validating arguments for numbers.
 *
 * @since 14 mai 2016
 * @author Gilles
 *
 * @param <N>
 *            The type of each number <code>Byte</code>, <code>Short</code>,
 *            <code>Integer</code>, <code>Long</code>, <code>Float</code>,
 *            <code>Double</code>, <code>BigInteger</code> or
 *            <code>BigDecimal</code>. Supports new <code>Number</code>, types
 *            only if it implements <code>Comparable</code>.
 */
public class AssertNumber<N extends Number & Comparable<N>> extends AssertObject<AssertNumber<N>, N> {

    /**
     * 
     * Constructor
     *
     * @param number
     *            the number to check
     */
    protected AssertNumber(final N number) {
        super(number);
    }

    /**
     * Assert that the first number is equal to the second one.
     * 
     * <pre>
     * AssertUtils.isEqual(10, 20);
     * </pre>
     * 
     * @param number
     *            The second number
     * @return this
     * @throws IllegalArgumentException
     *             if at least one number is {@code null} and if number are not
     *             equal.
     */
    public AssertNumber<N> isEqual(final N number) {
        return this.isEqual(number, (String) null);
    }

    /**
     * Assert that the first number is equal to the second one.
     * 
     * <pre>
     * AssertUtils.isEqual(10, 20, &quot;The numbers are not equal&quot;);
     * </pre>
     * 
     * @param number
     *            The second number
     * @param message
     *            the exception message, use the default assertion if null (%p
     *            or %1$p can be used to display parameter value, see
     *            explanation in the class description)
     * @param arguments
     *            the message arguments (use with String.format)
     * @return this
     * @throws IllegalArgumentException
     *             if at least one number is {@code null} and if number are not
     *             equal.
     */
    public AssertNumber<N> isEqual(final N number, final String message, final Object... arguments) {
        isEqual(this.get(), number, null, message, arguments);

        return this.getThis();
    }

    /**
     * Assert that the first number is equal to the second one.
     * 
     * <pre>
     * AssertUtils.isEqual(10, 20, exceptionToThrowOnError);
     * </pre>
     * 
     * @param number
     *            The second number message the exception message, use the
     *            default assertion if null
     * @param exception
     *            the exception to throw on error
     * @return this
     * @param <E>
     *            The type of exception
     * @throws E
     *             if at least one number is {@code null} and if number are not
     *             equal. The standard exception is appended as suppressed.
     */
    public <E extends Throwable> AssertNumber<N> isEqual(final N number, final E exception) throws E {
        isEqual(this.get(), number, exception, null);

        return this.getThis();
    }

    protected static <N extends Number & Comparable<N>, E extends Throwable> void isEqual(final N number1, final N number2,
            final E exception, final String message, final Object... arguments) throws E {
        if (compareNumber(number1, number2, exception, null) != 0) {
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
     * @param number
     *            The second number
     * @return this
     * @throws IllegalArgumentException
     *             if at least one number is {@code null} and if number are
     *             equal.
     */
    public AssertNumber<N> isNotEqual(final N number) {
        return this.isNotEqual(number, (String) null);
    }

    /**
     * Assert that the first number is not equal to the second one.
     * 
     * <pre>
     * AssertUtils.isNotEqual(10, 20, &quot;The numbers are equal&quot;);
     * </pre>
     * 
     * @param number
     *            The second number
     * @param message
     *            the exception message, use the default assertion if null (%p
     *            or %1$p can be used to display parameter value, see
     *            explanation in the class description)
     * @param arguments
     *            the message arguments (use with String.format)
     * @return this
     * @throws IllegalArgumentException
     *             if at least one number is {@code null} and if number are
     *             equal.
     */
    public AssertNumber<N> isNotEqual(final N number, final String message, final Object... arguments) {
        isNotEqual(this.get(), number, null, message, arguments);

        return this.getThis();
    }

    /**
     * Assert that the first number is not equal to the second one.
     * 
     * <pre>
     * AssertUtils.isNotEqual(10, 20, exceptionToThrowOnError);
     * </pre>
     * 
     * @param number
     *            The second number message the exception message, use the
     *            default assertion if null
     * @param exception
     *            the exception to throw on error
     * @return this
     * @param <E>
     *            The type of exception
     * @throws E
     *             if at least one number is {@code null} and if number are
     *             equal. The standard exception is appended as suppressed.
     */
    public <E extends Throwable> AssertNumber<N> isNotEqual(final N number, final E exception) throws E {
        isNotEqual(this.get(), number, exception, null);

        return this.getThis();
    }

    protected static <N extends Number & Comparable<N>, E extends Throwable> void isNotEqual(final N number1, final N number2,
            final E exception, final String message, final Object... arguments) throws E {

        if (compareNumber(number1, number2, exception, message, arguments) == 0) {
            manageExceptions("number1 is equal to number2.", exception, message, new Object[] {number1, number2}, arguments);
        }
    }

    /**
     * Assert that the first number is greater than the second one.
     * 
     * <pre>
     * AssertUtils.isGT(10, 20);
     * </pre>
     * 
     * @param number
     *            The second number
     * @return this
     * @throws IllegalArgumentException
     *             If at least one number is {@code null} and if number1 is not
     *             greater than number2.
     */
    public AssertNumber<N> isGT(final N number) {
        return this.isGT(number, (String) null);
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
     * @param number
     *            The second number
     * @param message
     *            the exception message, use the default assertion if null (%p
     *            or %1$p can be used to display parameter value, see
     *            explanation in the class description)
     * @param arguments
     *            the message arguments (use with String.format)
     * @return this
     * @throws IllegalArgumentException
     *             If at least one number is {@code null} and if number is not
     *             greater than number2
     */
    public AssertNumber<N> isGT(final N number, final String message, final Object... arguments) {
        isGT(this.get(), number, null, message, arguments);

        return this;
    }

    /**
     * Assert that the first number is greater than the second one.
     * 
     * <pre>
     * AssertUtils.isGT(10, 20, exceptionToThrowOnError);
     * </pre>
     * 
     * @param number
     *            The second number
     * @param exception
     *            the exception to throw on error
     * @return this
     * @param <E>
     *            The type of exception
     * @throws E
     *             If at least one number is {@code null} and if number is not
     *             greater than number2. The standard exception is appended as
     *             suppressed.
     */
    public <E extends Throwable> AssertNumber<N> isGT(final N number, final E exception) throws E {
        isGT(this.get(), number, exception, null);

        return this;
    }

    protected static <N extends Number & Comparable<N>, E extends Throwable> void isGT(final N number1, final N number2, final E exception,
            final String message, final Object... arguments) throws E {
        if (compareNumber(number1, number2, exception, message, arguments) <= 0) {
            manageExceptions("number1 is not greater than number2.", exception, message, new Object[] {number1, number2}, arguments);
        }
    }

    /**
     * Assert that the first number is greater than or equal to the second one.
     * 
     * @param number
     *            The second number
     * @return this
     * @throws IllegalArgumentException
     *             if at least one number is {@code null} and if number1 is
     *             lower than number2.
     */
    public AssertNumber<N> isGTE(final N number) {
        return this.isGTE(number, (String) null);
    }

    /**
     * Assert that the first number is greater than or equal to the second one.
     * 
     * @param number
     *            The second number
     * @param message
     *            the exception message, use the default assertion if null (%p
     *            or %1$p can be used to display parameter value, see
     *            explanation in the class description)
     * @param arguments
     *            the message arguments (use with String.format)
     * @return this
     * @throws IllegalArgumentException
     *             if at least one number is {@code null} and if number1 is
     *             lower than number2.
     */
    public AssertNumber<N> isGTE(final N number, final String message, final Object... arguments) {
        isGTE(this.get(), number, null, message, arguments);

        return getThis();
    }

    /**
     * Assert that the first number is greater than or equal to the second one.
     * 
     * @param number
     *            The second number
     * @param exception
     *            the exception to throw on error
     * @return this
     * @param <E>
     *            The type of exception
     * @throws E
     *             if at least one number is {@code null} and if number1 is
     *             lower than number2. The standard exception is appended as
     *             suppressed.
     */
    public <E extends Throwable> AssertNumber<N> isGTE(final N number, final E exception) throws E {
        isGTE(this.get(), number, exception, null);

        return this;
    }

    protected static <N extends Number & Comparable<N>, E extends Throwable> void isGTE(final N number1, final N number2, final E exception,
            final String message, final Object... arguments) throws E {
        if (compareNumber(number1, number2, exception, message, arguments) < 0) {
            manageExceptions("number1 is not greater than or equal to number2.", exception, message, new Object[] {number1, number2},
                    arguments);
        }
    }

    /**
     * Assert that the first number is lower than the second one.
     * 
     * @param number
     *            The second number
     * @return this
     * @throws IllegalArgumentException
     *             if at least one number is {@code null} and if number1 is not
     *             lower than number2.
     */
    public AssertNumber<N> isLT(final N number) {
        return this.isLT(number, (String) null);
    }

    /**
     * Assert that the first number is lower than the second one.
     * 
     * @param number
     *            The second number
     * @param message
     *            the exception message, use the default assertion if null (%p
     *            or %1$p can be used to display parameter value, see
     *            explanation in the class description)
     * @param arguments
     *            the message arguments (use with String.format)
     * @return this
     * @throws IllegalArgumentException
     *             if at least one number is {@code null} and if number1 is not
     *             lower than number2.
     */
    public AssertNumber<N> isLT(final N number, final String message, final Object... arguments) {
        isLT(this.get(), number, null, message, arguments);

        return this;
    }

    /**
     * Assert that the first number is lower than the second one.
     * 
     * @param number
     *            The second number
     * @param exception
     *            the exception to throw on error
     * @return this
     * @param <E>
     *            The type of exception
     * @throws E
     *             if at least one number is {@code null} and if number1 is not
     *             lower than number2. The standard exception is appended as
     *             suppressed.
     */
    public <E extends Throwable> AssertNumber<N> isLT(final N number, final E exception) throws E {
        isLT(this.get(), number, exception, null);

        return this;
    }

    protected static <N extends Number & Comparable<N>, E extends Throwable> void isLT(final N number1, final N number2, final E exception,
            final String message, final Object... arguments) throws E {
        if (compareNumber(number1, number2, exception, message, arguments) >= 0) {
            manageExceptions("number1 is not not lower than number2.", exception, message, new Object[] {number1, number2}, arguments);
        }
    }

    /**
     * Assert that the first number is lower than or equal to the second one.
     * 
     * @param number
     *            The second number
     * @return this
     * @throws IllegalArgumentException
     *             if at least one number is {@code null} and if number1 is
     *             greater than number2.
     */
    public AssertNumber<N> isLTE(final N number) {
        return this.isLTE(number, (String) null);
    }

    /**
     * Assert that the first number is lower than or equal to the second one.
     * 
     * @param number
     *            The second number
     * @param message
     *            the exception message, use the default assertion if null (%p
     *            or %1$p can be used to display parameter value, see
     *            explanation in the class description)
     * @param arguments
     *            the message arguments (use with String.format)
     * @return this
     * @throws IllegalArgumentException
     *             if at least one number is {@code null} and if number1 is
     *             greater than number2.
     */
    public AssertNumber<N> isLTE(final N number, final String message, final Object... arguments) {
        isLTE(this.get(), number, null, message, arguments);

        return this;
    }

    /**
     * Assert that the first number is lower than or equal to the second one.
     * 
     * @param number
     *            The second number
     * @param exception
     *            the exception to throw on error
     * @return this
     * @param <E>
     *            The type of exception
     * @throws E
     *             if at least one number is {@code null} and if number1 is
     *             greater than number2. The standard exception is appended as
     *             suppressed.
     */
    public <E extends Throwable> AssertNumber<N> isLTE(final N number, final E exception) throws E {
        isLTE(this.get(), number, exception, null);

        return this;
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
     * @param <E>
     *            The type of exception
     * @throws E
     *             if at least one number is {@code null} and if number1 is
     *             greater than number2. The standard exception is appended as
     *             suppressed.
     */
    protected static <N extends Number & Comparable<N>, E extends Throwable> void isLTE(final N number1, final N number2, final E exception,
            final String message, final Object... arguments) throws E {
        if (compareNumber(number1, number2, exception, message, arguments) > 0) {
            manageExceptions("number1 is not lower than or equal to number2.", exception, message, new Object[] {number1, number2},
                    arguments);
        }
    }

    private static <N extends Number & Comparable<N>, E extends Throwable> int compareNumber(final N number1, final N number2,
            final E exception, final String message, final Object... arguments) throws E {
        if (number1 != null && number2 != null) {
            return number1.compareTo(number2);
        } else if (number1 != null) {
            manageExceptions("number1 is null.", exception, message, new Object[] {number1, number2}, arguments);
        } else if (number2 != null) {
            manageExceptions("number2 is null.", exception, message, new Object[] {number1, number2}, arguments);
        } else {
            manageExceptions("number1 and number2 are null.", exception, message, new Object[] {number1, number2}, arguments);
        }
        throw exception;
    }
}